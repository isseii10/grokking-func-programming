val rawShows = List(
  "Breaking Bad (2008-2013)",
  "The Wire (2002-2008)",
  "Mad Men (2007-2015)"
)

val invalidRawShow = "Breaking Bad, 2008-2013"

case class TvShow(title: String, start: Int, end: Int)

def sortShows(shows: List[TvShow]): List[TvShow] = {
  shows.sortBy(show => show.end - show.start).reverse
}

// List[String]をList[TvShow]にperseする

def parseShows(rawShows: List[String]): List[TvShow] = {
  rawShows.map(rawShow => parseShow(rawShow))
}

// 想定したformatなら返せるが、それ以外はエラーになるので、
// def parseShow(rawShow: String): TvShow このシグネチャは
// stringを渡せばTvShowが返ってくることが期待される関数なのにTvShowが返ってこない場合があるのはよくない
def parseShow(rawShow: String): TvShow = {
  val bracketOpenIdx = rawShow.indexOf('(')
  val bracketCloseIdx = rawShow.indexOf(')')
  val dashIdx = rawShow.indexOf('-')

  val name = rawShow.substring(0, bracketOpenIdx).trim
  val yearStart =
    Integer.parseInt(rawShow.substring(bracketOpenIdx + 1, dashIdx))
  val yearEnd =
    Integer.parseInt(rawShow.substring(dashIdx + 1, bracketCloseIdx))
  TvShow(name, yearStart, yearEnd)
}
// 下のようにそれぞれでparseする方法もあるかもだが、これはシグネチャからは結局例外が変えるのが分からないし、それは問題
// def parseShow(rawShow: String): TvShow = {
//   val name = extractName(rawShow)
//   val yearStart = extractYearStart(rawShow)
//   val yearEnd = extractYearEnd(rawShow)
//   TvShow(name, yearStart, yearEnd)
// }
// def extractName(rawShow: String): String = {}
// def extractYearStart(rawShow: String): Int = {}
// def extractYearEnd(rawShow: String): Int = {}

// Optionを返すようにしてシグネチャから挙動がわかるようにする(Noneが返ってくることがある)
def parseShowV2(rawShow: String): Option[TvShow] = {
  for {
    name <- extractName(rawShow)
    yearStart <- extractYearStart(rawShow)
    yearEnd <- extractYearEnd(rawShow)
  } yield TvShow(name, yearStart, yearEnd)
}

def extractName(rawShow: String): Option[String] = {
  val bracketOpenIdx = rawShow.indexOf('(')
  for {
    name <-
      if (1 < bracketOpenIdx) Some(rawShow.substring(0, bracketOpenIdx).trim)
      else None
  } yield name
}

def extractYearStart(rawShow: String): Option[Int] = {
  val bracketOpenIdx = rawShow.indexOf('(')
  val dashIdx = rawShow.indexOf('-')
  for {
    yearStr <-
      if (bracketOpenIdx != -1 && bracketOpenIdx + 1 < dashIdx)
        Some(rawShow.substring(bracketOpenIdx + 1, dashIdx))
      else None
    // 右辺(?)は正しい時Option[String]になる。 '<-' の意味は右辺の(flatMapが実装されている)型の要素を一つ一つ取り出すのでyearStrはString
    year <- yearStr.toIntOption
  } yield year
}
def extractYearEnd(rawShow: String): Option[Int] = {
  val dashIdx = rawShow.indexOf('-')
  val bracketCloseIdx = rawShow.indexOf(')')
  for {
    yearStr <-
      if (dashIdx != -1 && dashIdx + 1 < bracketCloseIdx)
        Some(rawShow.substring(dashIdx + 1, bracketCloseIdx))
      else None
    year <- yearStr.toIntOption
  } yield year
}

// "title (1995)"のformatもokな要件が追加されたとする。
def parseShowV3(rawShow: String): Option[TvShow] = {
  for {
    name <- extractName(rawShow)
    yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
    yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
  } yield TvShow(name, yearStart, yearEnd)
}

def extractSingleYear(rawShow: String): Option[Int] = {
  val dashIdx = rawShow.indexOf('-')
  val bracketOpenIdx = rawShow.indexOf('(')
  val bracketCloseIdx = rawShow.indexOf(')')
  for {
    yearStr <-
      if (
        dashIdx == -1 && bracketOpenIdx != -1 && bracketOpenIdx < bracketCloseIdx
      )
        Some(rawShow.substring(bracketOpenIdx + 1, bracketCloseIdx))
      else None
    year <- yearStr.toIntOption
  } yield year
}

// orElse練習
// def extractSingleYearOrYearEnd(rawShow: String): Option[Int] = {
//   extractSingleYear(rawShow).orElse(extractYearEnd(rawShow))
// }
// def extractAnyYear(rawShow: String): Option[Int] = {
//   extractYearStart(rawShow)
//     .orElse(extractYearEnd(rawShow))
//     .orElse(extractSingleYear(rawShow))
// }
//
// def extractSingleYearIfTitleExists(rawShow: String): Option[Int] = {
//   extractName(rawShow).flatMap(name => extractSingleYear(rawShow))
// }
//
// def extractAnyYearIfTitleExists(
//     rawShow: String
// ): Option[Int] = {
//   extractName(rawShow)
//     .flatMap(name => extractAnyYear(rawShow))
// }

// List[Option[TvShow]]を返すようになったので、parseShowsも変更する必要がある。
// が、この修正方針はNoneを無視することになる。(エラーを握り潰している)
def parseShowsV3(rawShows: List[String]): List[TvShow] = {
  for {
    raw <- rawShows
    parsed <- parseShowV3(raw)
  } yield parsed
}

// all or nothingのエラー処理方針
def addOrResign(
    parsedShows: Option[List[TvShow]],
    newParsedShow: Option[TvShow]
): Option[List[TvShow]] = {
  for {
    shows <- parsedShows // Option[List[TvShow]]からList[TvShow]を取り出す
    newShow <- newParsedShow // Option[TvShow]からTvShowを取り出す
  } yield shows.appended(newShow) // ジェネレータがOption[]なので最後もOptionになる
}
// flatMapで書き直してみた
def _addOrResign(
    parsedShows: Option[List[TvShow]],
    newParsedShow: Option[TvShow]
): Option[List[TvShow]] = {
  parsedShows.flatMap(shows =>
    newParsedShow.map(newShow => shows.appended(newShow))
  )
}
