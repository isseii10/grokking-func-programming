package chapter11

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
    case Artist(name: String, follower: Int)
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

def travelGuide(
    data: DataAccess,
    attractionName: String
): IO[Option[TravelGuide]] = {
  for {
    attractions <- data.findAttractions(attractionName, ByLocationPopulation, 1)
    guide <- attractions.headOption match {
      case None => IO.pure(None)
      case Some(attraction) =>
        for {
          artists <- data.findArtistsFromLocation(attraction.location.id, 2)
          movies <- data.findMoviesAboutLocation(attraction.location.id, 2)
        } yield Some(TravelGuide(attraction, artists.appendedAll(movies)))
    }
  } yield guide
}
