val rawShows = List(
  "Breaking Bad (2008-2013)",
  "The Wire (2002-2008)",
  "Mad Men (2007-2015)"
)

case class TvShow(title: String, start: Int, end: Int)

def sortShows(shows: List[TvShow]): List[TvShow] = {
  shows.sortBy(show => show.end - show.start).reverse
}

// List[String]をList[TvShow]にperseする

def parseShows(rawShows: List[String]): List[TvShow] = {
  rawShows.map(parseShow)
}

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
