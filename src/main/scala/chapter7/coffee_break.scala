// プレイリストのモデル化練習
object model {
  opaque type User = String
  object User {
    def apply(user: String): User = user
  }
  opaque type Artist = String
  object Artist {
    def apply(artist: String): Artist = artist
  }
  enum Kind {
    case CreatedByUser(user: User)
    case ParticularArtist(artist: Artist)
    case ParticularGenres(genres: List[MusicGenre])
  }
  enum MusicGenre {
    case Funk
    case House
    case Rock
  }

}
import model._
import model.Kind._
import model.MusicGenre._

case class Song(name: String, artist: Artist)
case class Playlist(name: String, kind: Kind, songs: List[Song])

val fooFighters = Artist("Foo Fighters")
val playList1 = Playlist(
  "This is Foo Fighters",
  ParticularArtist(fooFighters),
  List(Song("Breakout", fooFighters), Song("Learn To Fly", fooFighters))
)

val playList2 = Playlist(
  "Deep Focus",
  ParticularGenres(List(Funk, House)),
  List(
    Song("One More Time", Artist("Daft Punk")),
    Song("Hey Boy Hey Girl", Artist("Chemical Brothers"))
  )
)

def gatherSongs(
    playLists: List[Playlist],
    artist: Artist,
    genre: MusicGenre
): List[Song] = {
  playLists.foldLeft(List.empty[Song])((songs, playlist) =>
    val matchingSongs = playlist.kind match {
      case CreatedByUser(user) => playlist.songs.filter(_.artist == artist)
      case ParticularArtist(playlistArtist) =>
        if (playlistArtist == artist) playlist.songs else List.empty
      case ParticularGenres(genres) =>
        if (genres.contains(genre)) playlist.songs else List.empty
    }
    songs.appendedAll(matchingSongs)
  )
}
