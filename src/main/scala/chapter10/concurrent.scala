object cityModel {
  opaque type City = String
  object City {
    def apply(name: String): City = name
    extension (city: City) def name: String = city
  }
  case class CityStats(city: City, checkIns: Int)
}
import cityModel._
import cats.effect._
import cats.effect.unsafe.implicits.global
import fs2._
import cats.implicits.catsSyntaxParallelSequence1
import scala.concurrent.duration._

// test code
val checkIns: Stream[IO, City] = Stream(
  City("Sydney"),
  City("Sydney"),
  City("Cape Town"),
  City("Singapore"),
  City("Cape Town"),
  City("Sydney")
).covary[IO]

val checkIns2: Stream[IO, City] = Stream(
  City("Sydney"),
  City("Dublin"),
  City("Cape Town"),
  City("Lima"),
  City("singapore")
).repeatN(100_000)
  .append(Stream.range(0, 100_000).map(i => City(s"City $i")))
  .append(Stream(City("Sydney"), City("Sydney"), City("Lima")))
  .covary[IO]

case class ProcessingCheckIns(
    currentRanking: IO[List[CityStats]],
    stop: IO[Unit]
)

def processCheckIns(checkIns: Stream[IO, City]): IO[ProcessingCheckIns] = {
  // chunkで区切ったバッチ処理のバージョン
  // checkIns
  //   .scan(Map.empty[City, Int])((map, city) =>
  //     map.updated(city, map.getOrElse(city, 0) + 1)
  //   )
  //   .chunkN(100_000)
  //   .map(_.last)
  //   .unNone
  //   .map(topCities)
  //   .foreach(IO.println)
  //   .compile
  //   .drain
  for {
    storedCheckIns <- Ref.of[IO, Map[City, Int]](Map.empty)
    storedRanking <- Ref.of[IO, List[CityStats]](List.empty)
    rankingProgram = updateRanking(storedCheckIns, storedRanking)
    checkInsProgram = checkIns
      .evalMap(storeCheckIn(storedCheckIns))
      .compile
      .drain
    // outputProgram = IO  // storedRankingを取得することができるので、outputProgramはもはやいらない
    //   .sleep(1.second)
    //   .flatMap(_ => storedRanking.get)
    //   .flatMap(IO.println)
    //   .foreverM
    fiber <- List(
      rankingProgram,
      checkInsProgram
      // outputProgram
    ).parSequence.start
  } yield ProcessingCheckIns(storedRanking.get, fiber.cancel)
}

def updateRanking(
    storedCheckIns: Ref[IO, Map[City, Int]],
    storedRanking: Ref[IO, List[CityStats]]
): IO[Nothing] = {
  for {
    newRanking <- storedCheckIns.get.map(topCities)
    _ <- storedRanking.set(newRanking)
    result <- updateRanking(
      storedCheckIns,
      storedRanking
    )
    // ここは再帰で終わらないが、resultはNothing型であり
    // IO[Nothing]を返すことで終わらない関数ということをシグネチャで分からせる。
    // IO[Nothing]は実行時にプログラムから制御が戻らないか、失敗することを意味する。
  } yield result
}

def storeCheckIn(storedCheckIns: Ref[IO, Map[City, Int]])(
    city: City
): IO[Unit] = {
  storedCheckIns.update(_.updatedWith(city)(_ match {
    case None           => Some(1)
    case Some(checkIns) => Some(checkIns + 1)
  }))
}

def topCities(cityCheckIns: Map[City, Int]): List[CityStats] = {
  cityCheckIns.toList
    .map(_ match {
      case (city, checkIns) => CityStats(city, checkIns)
    })
    .sortBy(_.checkIns)
    .reverse
    .take(3)
}
