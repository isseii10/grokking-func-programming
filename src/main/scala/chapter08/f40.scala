import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.effect.unsafe.implicits._
import cats.syntax.traverse._
import cats.syntax.either._

def f01[A, B](x: IO[A], f: A => B): IO[B] = x.map(f)

def f02[A](x: IO[IO[A]]): IO[A] = x.flatten

def f03[A, B](x: IO[A], f: A => IO[B]): IO[B] = x.map(f).flatten

def f04[A](x: A): IO[A] = IO.pure(x)

def f05[A](impureAction: () => A): IO[A] = IO.delay(impureAction())

def f06[A](x: IO[A], alternative: IO[A]) = x.orElse(alternative)

def f07[A](x: List[IO[A]]): IO[List[A]] = x.sequence

def f08[A](x: Option[IO[A]]): IO[Option[A]] = x.sequence

def f09[A](x: List[A], y: List[A]): List[A] = x.appendedAll(y)

def f10[A](x: List[A], f: A => Boolean): List[A] = x.filter(f)

def f11[A](x: List[A], zero: A, f: (A, A) => A): A =
  x.foldLeft(zero)((acc, a) => f(acc, a))

def f12[A](x: List[List[A]]): List[A] = x.flatten

def f13[A, B](x: List[A], f: A => List[B]): List[B] =
  x.map(f).flatten // flatMap(f)

def f14[A](x: List[A], f: A => Boolean): Boolean = x.forall(f) // x.exists(f)

def f15[A, B](x: Set[A], f: A => B): Set[B] = x.map(f)

def f16[A](x: Set[A], f: A => Boolean): Set[A] = x.filter(f)

def f17[A](x: Set[A], zero: A, f: (A, A) => A): A =
  x.foldLeft(zero)((acc, a) => f(acc, a))

def f18[A](x: Set[Set[A]]): Set[A] = x.flatten

def f19[A, B](x: Set[A], f: A => Set[B]): Set[B] = x.flatMap(f)

def f20[A](x: Set[A], f: A => Boolean): Boolean = x.exists(f)

def f21[A, B](x: Option[A], f: A => B): Option[B] = x.map(f)

def f22[A](x: Option[A], f: A => Boolean): Option[A] = x.filter(f)

def f23[A, B](x: Option[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)

def f24[A](x: Option[Option[A]]): Option[A] = x.flatten

def f25[A, B](x: Option[A], f: A => Option[B]): Option[B] = x.flatMap(f)

def f26[A](x: Option[A], f: A => Boolean): Boolean = x.forall(f)

def f27(x: String): Option[Int] = x.toIntOption

def f28[A](x: Option[A], alternative: Option[A]): Option[A] =
  x.orElse(alternative)

// def f29[A, B](x: Option[A], y: B): Either[B, A] = x match {
//   case Some(a) => Right(a)
//   case None    => Left(y)
// }
def f29[A, B](x: Option[A], y: B): Either[B, A] = x.toRight(y)
// def f30[A, B](x: Option[A], y: B): Either[A, B] = x match {
//   case Some(a) => Left(a)
//   case None    => Right(y)
// }
def f30[A, B](x: Option[A], y: B): Either[A, B] = x.toLeft(y)

def f31[A](x: List[Option[A]]): Option[List[A]] = x.sequence

// def f32[A, B, C](x: Either[A, B], f: B => C): Either[A, C] = x match {
//   case Right(r) => Right(f(r))
//   case Left(l)  => Left(l)
// }
def f32[A, B, C](x: Either[A, B], f: B => C): Either[A, C] = x.map(f)

def f33[A, B, C](x: Either[A, B], zero: C, f: (C, B) => C): C =
  x.foldLeft(zero)(f)

def f34[A, B](x: Either[A, Either[A, B]]): Either[A, B] = x.flatten

def f35[A, B, C](x: Either[A, B], f: B => Either[A, C]): Either[A, C] =
  x.flatMap(f)

def f36[A, B](x: Either[A, B], f: B => Boolean): Boolean = x.forall(f)

def f37[A, B](x: Either[A, B], alternative: Either[A, B]): Either[A, B] =
  x.orElse(alternative)

def f38[A, B](x: Either[A, B]): Option[B] = x.toOption

def f39[A, B](x: List[Either[A, B]]): Either[A, List[B]] = x.sequence

def f40[A, B](x: Either[A, List[B]]): List[Either[A, B]] = x.sequence
