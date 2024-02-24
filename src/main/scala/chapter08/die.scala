import cats.effect.IO
import cats.effect.unsafe.implicits.global

def castTheDieInpure(): Int = ???
def castTheDie(): IO[Int] = IO.delay(castTheDieInpure())

def castTheDieTwice(): IO[Int] = {
  for {
    first <- castTheDie()
    second <- castTheDie()
  } yield first + second
}
// IOにもflatMapがある
// IO[A]のAを取り出して、それを演算したりできる
