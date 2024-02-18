// 要件: 音楽アーティストのカタログ
// 音楽アーティストのリストを検索できなくてはならない
// 各検索では、ジャンル、出身地、活動期間など、条件の様々な組み合わせをサポートしなければならない
// 音楽アーティストには名前、ジャンル、出身地、活動開始年、活動停止年がある

object model {
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
  opaque type YearActiveStart = Int
  object YearActiveStart {
    def apply(s: Int): YearActiveStart = s
    extension (
        a: YearActiveStart
    ) def value: Int = a
  }
  opaque type YearActiveEnd = Int
  object YearActiveEnd {
    def apply(s: Int): YearActiveEnd = s
    extension (
        a: YearActiveEnd
    ) def value: Int = a
  }
}

import model._

case class Artist(
    name: String,
    genre: String,
    origin: Location,
    yearsActiveStart: Int,
    yearsActiveEnd: Option[Int]
)

def searchArtists(
    artists: List[Artist],
    genres: List[String],
    locations: List[String],
    searchByActiveYear: Boolean,
    activeAfter: Int,
    activeBefore: Int
): List[Artist] = {
  artists.filter(artist =>
    (genres.isEmpty || genres.contains(artist.genre)) &&
      (locations.isEmpty || locations.contains(artist.origin.name)) &&
      (!searchByActiveYear || (artist.yearsActiveEnd
        .forall(_ >= activeAfter)) && (artist.yearsActiveStart <= activeBefore))
  )
}
