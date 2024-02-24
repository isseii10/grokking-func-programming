def exchangeRatesTableApiCall(currency: String): Map[String, BigDecimal] = ???

object currencyModel {
  opaque type Currency = String
  object Currency {
    def apply(s: String): Currency = s
    extension (currency: Currency) def name: String = currency
  }
}

import currencyModel._
import cats.effect.IO

def trending(rates: List[BigDecimal]): Boolean = {
  rates.size > 1 // 1以下だと増加しているか分からないのでfalseを返す
  &&
  rates
    .zip(rates.drop(1))
    .forall(ratePair =>
      ratePair match {
        case (prev, now) => prev < now
      }
    )
  // 一つ前と今のrateの二つの情報が欲しい
  // 命令型だと for i:=1;i<len(rates);i++してi-1とiを同じratesから取るが、関数型はこの解決法ではない。
  // (prev, now)のタプルのリストを作ってその演算(大小比較)を行えるようにする
  // おそらく関手とかの知識があれば理解できそう
}

def extractSingleCurrencyRate(
    currencyToExtract: Currency
)(table: Map[Currency, BigDecimal]): Option[BigDecimal] = {
  table
    .filter(kv =>
      kv match {
        case (currency, rate) => currency == currencyToExtract
      }
    )
    .values
    .headOption
}

def exchangeTable(from: Currency): IO[Map[Currency, BigDecimal]] = {
  IO.delay(exchangeRatesTableApiCall(from.name))
    .map(table =>
      table.map(kv =>
        kv match {
          case (currencyName, rate) => (Currency(currencyName), rate)
        }
      )
    )
}

import fs2._
import cats.syntax.all._
def rates(from: Currency, to: Currency): Stream[IO, BigDecimal] = {
  Stream
    .eval(exchangeTable(from))
    .repeat
    .map(extractSingleCurrencyRate(to))
    .unNone
    .orElse(rates(from, to))
}

import cats.syntax.traverse._
def lastRates(from: Currency, to: Currency, n: Int): IO[List[BigDecimal]] = {
  List.range(0, n).map(_ => currencyRate(from, to)).sequence
}
import scala.concurrent.duration._
import java.util.concurrent._
val delay: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
val ticks: Stream[IO, Unit] = Stream.fixedRate[IO](delay)

def exchangeIfTranding(
    amount: BigDecimal,
    from: Currency,
    to: Currency
): IO[BigDecimal] = {
  // for {
  //   rates <- lastRates(from, to, 3)
  //   result <-
  //     if (trending(rates)) IO.pure(amount * rates.last)
  //     else exchangeIfTranding(amount, from, to)
  // } yield result
  rates(from, to)
    .zipLeft(ticks) // zipのペアが揃うまでタプルが作られないため1秒間隔のticksとzipすることで1秒感覚でratesが呼ばれる
    .sliding(3) // sliding window
    .map(_.toList)
    .filter(trending)
    .map(_.last)
    .take(1)
    .compile
    .lastOrError
    .map(_ * amount)
}

def currencyRate(from: Currency, to: Currency): IO[BigDecimal] = {
  // exchangeTable(from) // IO[Map[Currency, BigDecimal]]
  //   .map(extractSingleCurrencyRate(to)) // IO[Option[BigDecimal]]
  //   .flatMap(_ match { // IO[IO[BigDecimal]]のflatMapなのでIO[BigDecimal]
  //     case Some(rate) => IO.pure(rate)
  //     case None       => currencyRate(from, to)
  //   })
  for {
    table <- retry(exchangeTable(from), 10)
    rate <- extractSingleCurrencyRate(to)(table) match {
      case Some(v) => IO.pure(v)
      case None    => currencyRate(from, to)
    }
  } yield rate
}
