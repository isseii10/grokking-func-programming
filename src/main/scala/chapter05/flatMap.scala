// 本に基づいて映画レコメンデーションのフィードを返す

// Step1: bookごとに著者を取得
// Step2: authorごとにbookAdaptions関数を呼び出してList[Movie]を取得
// Step3: movieごとにレコメンデーションフィード文字列を作成

// Step1: bookごとに著者を取得
case class Book(title: String, authors: List[String])

val books = List(
  Book("FP in Scala", List("Chiusano", "Bjarnason")),
  Book("The Hobbit", List("Tolkien"))
)
// books.map(_.authors)
// -> List(List("Chiusano", "Bjarnason"), List("Tolkien"))

// books.map(_authors).flatten
// -> List("Chiusano", "Bjarnason", "Tolkien")

// books.flatMap(_.authors)
// -> List("Chiusano", "Bjarnason", "Tolkien")
val authors = books.flatMap(_.authors)

// Step2: authorごとにbookAdaptions関数を呼び出してList[Movie]を取得
case class Movie(title: String)

def bookAdaptations(author: String): List[Movie] = {
  if (author == "Tolkien")
    List(Movie("An Unexpected Journey"), Movie("The Desolation of Smaug"))
  else List.empty
}

// val movies = authors.flatMap(bookAdaptations)
val movies = books.flatMap(_.authors).flatMap(bookAdaptations)

// Step3: movieごとにレコメンデーションフィード文字列を作成
// mapで処理すれば良いと思われるがautherとbook.titleはスコープが違うので見れない
val recommendations = books
  .flatMap(_.authors) // -> authors
  .flatMap(bookAdaptations) // -> movies
  .map(movie =>
    s"You may like ${movie.title} because you liked {author}'s {book.title}" // authorとbook.titleが取れない
  )

// 入れ子にすることで解決
val recommendationsV2 = books.flatMap(book =>
  book.authors.flatMap(author =>
    bookAdaptations(author).map(movie =>
      s"You may like ${movie.title} because you liked ${author}'s ${book.title}"
    )
  )
)

// for内包表記
val recommendationsV3 = {
  for {
    book <- books
    author <- book.authors
    movie <- bookAdaptations(author)
  } yield s"You may like ${movie.title} because you liked ${author}'s ${book.title}"
}
