// 要件: 音楽アーティストのカタログ
// 音楽アーティストのリストを検索できなくてはならない
// 各検索では、ジャンル、出身地、活動期間など、条件の様々な組み合わせをサポートしなければならない
// 音楽アーティストには名前、ジャンル、出身地、活動開始年、活動停止年がある

object modelArtist {
  opaque type Location = String
  object Location {
    def apply(s: String): Location = s // コンストラクタ
    extension (
        a: Location
    ) def name: String = a //  Location型にnameというmethodをつける。元のStringを返す
  }
  opaque type Genre = String
  object Genre {
    def apply(s: String): Genre = s
    extension (
        a: Genre
    ) def name: String = a
  }

}

enum MusicGenre {
  case HeavyMetal
  case Pop
  case HardRock
}

enum YearsActive {
  case StillActive(since: Int)
  case ActiveBetween(start: Int, end: Int)
}

import modelArtist._
import YearsActive._

case class Artist(
    name: String,
    genre: MusicGenre,
    origin: Location,
    yearsActive: YearsActive
)

def wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int): Boolean = {
  artist.yearsActive match
    case StillActive(since)        => since <= yearEnd
    case ActiveBetween(start, end) => start >= yearEnd && end >= yearStart
}

enum SearchCondition {
  case SearchByGenre(genres: List[MusicGenre])
  case SearchByOrigin(origins: List[Location])
  case SearchByActiveYear(start: Int, end: Int)
}

import SearchCondition._

def searchArtists(
    artists: List[Artist],
    requiredConditions: List[SearchCondition]
): List[Artist] = {
  artists.filter(artist =>
    requiredConditions.forall(requiredCondition =>
      requiredCondition match {
        case SearchByGenre(genres) =>
          genres.contains(artist.genre)
        case SearchByOrigin(origins) =>
          origins.contains(artist.origin)
        case SearchByActiveYear(start, end) =>
          wasArtistActive(artist, start, end)
      }
    )
  )
}

// パターンマッチング練習
def activeLength(artist: Artist, currentYear: Int): Int = {
  artist.yearsActive match
    case StillActive(sinse)        => currentYear - sinse
    case ActiveBetween(start, end) => end - start
}
