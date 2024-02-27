object travelGuideModel {
  opaque type LocationId = String
  object LocationId {
    def apply(id: String): LocationId = id
    extension (locationId: LocationId) def id(id: LocationId): String = id
  }
  case class Location(id: LocationId, name: String, population: Int)

  case class Attraction(
      name: String,
      description: Option[String],
      location: Location
  )

  enum PopCultureSubject {
    case Artist(name: String, followers: Int)
    case Movie(name: String, boxOffice: Int)
  }

  case class TravelGuide(
      attraction: Attraction,
      subjects: List[PopCultureSubject]
  )

  enum AttractionOrdering {
    case ByName
    case ByLocationPopulation
  }
}

import travelGuideModel._
import travelGuideModel.PopCultureSubject._
import travelGuideModel.AttractionOrdering._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

// データアクセス
trait DataAccess {
  def findArtistsFromLocation(
      locationId: LocationId,
      limit: Int
  ): IO[List[Artist]]

  def findMoviesAboutLocation(
      locationId: LocationId,
      limit: Int
  ): IO[List[Movie]]

  def findAttractions(
      name: String,
      ordering: AttractionOrdering,
      limit: Int
  ): IO[List[Attraction]]
}

def guideScore(guide: TravelGuide): Int = {
  val descriptionScore = guide.attraction.description.map(_ => 30).getOrElse(0)
  val quantityScore = Math.min(40, guide.subjects.size * 10)
  val totalFollowers = guide.subjects
    .map(_ match {
      case Artist(_, followers) => followers
      case _                    => 0
    })
    .sum
  val totalBoxOffice = guide.subjects
    .map(_ match {
      case Movie(_, boxOffice) => boxOffice
      case _                   => 0
    })
    .sum
  val followersScore = Math.min(15, totalFollowers / 100_000)
  val boxOfficeScore = Math.min(15, totalBoxOffice / 10_000_000)
  descriptionScore + quantityScore + followersScore + boxOfficeScore
}

import cats.Traverse.nonInheritedOps.toTraverseOps
def travelGuide(
    data: DataAccess,
    attractionName: String
): IO[Option[TravelGuide]] = {
  for {
    attractions <- data.findAttractions(attractionName, ByLocationPopulation, 3)
    guides <- attractions
      .map(attraction =>
        for {
          artists <- data.findArtistsFromLocation(attraction.location.id, 2)
          movies <- data.findMoviesAboutLocation(attraction.location.id, 2)
        } yield TravelGuide(attraction, artists.appendedAll(movies))
      )
      .sequence
  } yield guides.sortBy(guideScore).reverse.headOption
}

// 接続
import org.apache.jena.query._
import org.apache.jena.rdfconnection._
import cats.effect.Resource
val connectionResource: Resource[IO, RDFConnection] =
  Resource.make(
    IO.blocking(
      RDFConnectionRemote.create
        .destination("https://query.wikidata.org/")
        .queryEndpoint("sparql")
        .build
    )
  )(connection => IO.blocking(connection.close()))

val program: IO[Option[TravelGuide]] = connectionResource.use(connection => {
  val wikidata = getSparqlDataAccess(execQuery(connection))
  travelGuide(wikidata, "Yellowstone")
})

def createExecution(
    connection: RDFConnection,
    query: String
): IO[QueryExecution] =
  IO.blocking(connection.query(QueryFactory.create(query)))

def closeExecution(execution: QueryExecution): IO[Unit] =
  IO.blocking(execution.close())

import scala.jdk.javaapi.CollectionConverters.asScala
def execQuery(
    connection: RDFConnection
)(query: String): IO[List[QuerySolution]] = {
  val executionResource: Resource[IO, QueryExecution] =
    Resource.make(createExecution(connection, query))(closeExecution)
  executionResource.use(execution =>
    IO.blocking(asScala(execution.execSelect()).toList)
  )
}

def parseAttraction(s: QuerySolution): IO[Attraction] = {
  IO.delay(
    Attraction(
      name = s.getLiteral("attractionLabel").getString,
      description =
        if (s.contains("description"))
          Some(s.getLiteral("description").getString)
        else None,
      location = Location(
        id = LocationId(s.getResource("location").getLocalName),
        name = s.getLiteral("locationLabel").getString,
        population = s.getLiteral("population").getInt
      )
    )
  )
}

// DataAccessの実装
def getSparqlDataAccess(
    execQuery: String => IO[List[QuerySolution]]
): DataAccess = {
  val prefix = """
      |PREFIX wd: <http://www.wikidata.org/entity/>
      |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX schema: <http://schema.org/>
      |""".stripMargin

  new DataAccess {
    def findAttractions(
        name: String,
        ordering: AttractionOrdering,
        limit: Int
    ): IO[List[Attraction]] = {
      val orderBy = ordering match {
        case ByName               => "?attractionLabel"
        case ByLocationPopulation => "DESC(?population)"
      }

      val query = s"""
          |${prefix}
          |SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
          |  ?attraction wdt:P31 wd:Q570116;
          |    rdfs:label ?attractionLabel;
          |    wdt:P131 ?location.
          |  FILTER((LANG(?attractionLabel)) = "en")
          |  OPTIONAL {
          |    ?attraction schema:description ?description.
          |    FILTER((LANG(?description)) = "en")
          |  }
          |  ?location wdt:P1082 ?population;
          |    rdfs:label ?locationLabel.
          |  FILTER((LANG(?locationLabel)) = "en")
          |  FILTER(CONTAINS(?attractionLabel, "${name}"))
          |} ORDER BY ${orderBy} LIMIT ${limit}
          |""".stripMargin

      for {
        solutions <- execQuery(query)
        attractions <- solutions.traverse(parseAttractions)
      } yield attractions
    }

    def findArtistsFromLocation(
        locationId: LocationId,
        limit: Int
    ): IO[List[Artist]] = {
      val query = s"""
        |${prefix}
        |SELECT DISTINCT ?artist ?artistLabel ?followers WHERE {
        |  ?artist wdt:P136 ?genre;
        |    wdt:P8687 ?followers;
        |    rdfs:label ?artistLabel.
        |  FILTER(LANG(?artistLabel) = "en").
        |  ?artist wdt:P740 wd:${locationId.id}
        |} ORDER BY DESC(?followers) LIMIT ${limit}
        |""".stripMargin

      for {
        solutions <- execQuery(query)
        artists <-
          IO.delay {
            solutions.map[Artist] { solution =>
              Artist(
                name = solution.getLiteral("artistLabel").getString,
                followers = solution.getLiteral("followers").getInt
              )
            }
          }
      } yield artists
    }

    def findMoviesAboutLocation(
        locationId: LocationId,
        limit: Int
    ): IO[List[Movie]] = {
      val query = s"""
          |${prefix}
          |SELECT DISTINCT ?subject ?subjectLabel ?boxOffice WHERE {
          |  ?subject wdt:P31 wd:Q11424;
          |    wdt:P2142 ?boxOffice;
          |    rdfs:label ?subjectLabel.
          |  ?subject wdt:P840 wd:${locationId.id}
          |  FILTER(LANG(?subjectLabel) = "en").
          |} ORDER BY DESC(?boxOffice) LIMIT ${limit}
          |""".stripMargin

      for {
        solutions <- execQuery(query)
        movies <- IO.delay {
          solutions.map[Movie] { solution =>
            Movie(
              name = solution.getLiteral("subjectLabel").getString,
              boxOffice = solution.getLiteral("boxOffice").getInt
            )
          }
        }
      } yield movies
    }
  }
}

def parseAttractions(solution: QuerySolution): IO[Attraction] = {
  IO.delay {
    Attraction(
      name = solution.getLiteral("attractionLabel").getString(),
      description =
        if solution.contains("description")
        then Some(solution.getLiteral("description").getString())
        else None,
      location = Location(
        id = LocationId(solution.getResource("location").getLocalName()),
        name = solution.getLiteral("locationLabel").getString(),
        population = solution.getLiteral("population").getInt()
      )
    )
  }
}
