package slark
package parser

trait CombinatorAst { self: Parsers with CombinatorApi =>

  implicit class PrefixOps[T](t: T) {
    def apply[S](p: Parser[S])(implicit num: Integral[T]): Repeated[S] = Repeated[S](num.toInt(t), p)
  }

  implicit class BinaryOps[T](self: T) {
    def |[S, T_, S_ >: S](that: T_)(implicit fn1: Builder[T, S], fn2: Builder[T_, S_]): Parser[S_] =
      alt(fn1(self), fn2(that))

    def ^[T_, S, S_](that: T_)(implicit fn1: Builder[T, S], fn2: Builder[T_, S_]): Parser[(S, S_)] =
      seq(fn1(self), fn2(that))

    def ^:[S, T_, S_](that: T_)(implicit fn1: Builder[T, S], fn2: Builder[T_, S_]): Parser[S_] =
      map(seq(fn2(that), fn1(self)))(_._1)

    def :^[S, T_, S_](that: T_)(implicit fn1: Builder[T, S], fn2: Builder[T_, S_]): Parser[S_] =
      map(seq(fn1(self), fn2(that)))(_._2)
  }

  implicit class SuffixOps[T](self: T) {
    def !(implicit fn: Builder[T, _]): Parser[Unit] = not(fn(self))

    def ?[S](implicit fn: Builder[T, S]): Parser[Option[S]] = map(rep[S](fn(self))(_ < 1)(_ => true))(_.headOption)

    def *[S](implicit fn: Builder[T, S]): Parser[List[S]] = rep[S](fn(self))(_ == Int.MaxValue)(_ => true)
  }

  implicit class FunctOps[S](p: Parser[S]) {
    def ->[S_](fn: S => S_): Parser[S_] = map(p)(fn)

    def >>[S_](fn: S => Parser[S_]): Parser[S_] = flatmap(p)(fn)
  }

  case class Repeated[S](count: Int, parser: Parser[S]) {
    require(count >= 0)

    def + : Parser[List[S]] = rep[S](parser)(_ == Int.MaxValue)(_ >= count)

    def - : Parser[List[S]] = {
      require(count > 0)
      rep(parser)(_ == count)(_ => true)
    }

    def fixed: Parser[List[S]] = {
      require(count > 0)
      rep(parser)(_ == count)(_ == count)
    }

  }

  object Repeated {
    import scala.language.implicitConversions
    private[this] val singletonRepeatedToParser: (Repeated[Any] => Parser[List[Any]]) = _.fixed
    implicit def RepeatedToParser[S]: (Repeated[S] => Parser[List[S]]) =
      singletonRepeatedToParser.asInstanceOf[Repeated[S] => Parser[List[S]]]
  }
}