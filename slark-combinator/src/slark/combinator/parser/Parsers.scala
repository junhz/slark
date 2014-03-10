package slark
package combinator.parser

import scala.annotation.tailrec

trait Parsers { parsers =>
  type Input <: AnyRef

  sealed trait ParseResult[+S] {
  }

  case class Fail(msg: String) extends ParseResult[Nothing]

  case class Succ[+S](result: S, next: Input) extends ParseResult[S]

  sealed abstract class Parser[+S] { self =>
    type Result = ParseResult[S]

    def parse(input: Input): Result

    /** flatmap */
    def >>[T](fn: S => Parser[T]): Parser[T]

    /** plus */
    def |[T >: S](that: Parser[T]): Parser[T]

    /** map */
    final def ->[T](fn: S => T): Parser[T] = self >> Cache(Parsers.this) { x => succ(fn(x)) }

    /** seq */
    final def ^[T](that: Parser[T]): Parser[(S, T)] = self >> { x => that -> { y => (x, y) } }

    /** guard */
    final def :^[T](that: Parser[T]): Parser[T] = self >> { _ => that }

    /** guard */
    final def ^:[T](that: Parser[T]): Parser[T] = that >> { x => self -> { _ => x } }

    /** not */
    final def ! : Parser[Unit] = ((self >> Cache(Parsers.this) { x: S => succ(fail("missing an expected failure")) }) | succ(succ())) >> { p => p }

    /** rep */
    final def * : Parser[List[S]] = (self >> { x => self.* -> { xs => x :: xs } }) | succ(Nil)

    /** rep */
    final def apply(time: Int): Parser[List[S]] =
      if (time > 0) self >> { x => self { time - 1 } -> { xs => x :: xs } }
      else succ(Nil)

    /** option */
    final def ? : Parser[Option[S]] = (self -> { x => Some(x) }) | succ(None)

    /** option rep */
    /*final def ?(time: Int): Parser[List[S]] =
      if (time > 0) (self >> { x => self.?{ time - 1 } -> { xs => x :: xs } }) | succ(Nil)
      else throw new IllegalArgumentException("repeat time should  be greater then 0")*/
  }

  final def parser[S](fn: Input => ParseResult[S]): Parser[S] = StateParser(input => fn(input))

  case class StateParser[S](lazyParse: Input => AnyRef) extends Parser[S] {
    override def parse(input: Input) = {
      @tailrec
      def run(a: AnyRef): AnyRef =
        a match {
          case r: ParseResult[AnyRef] => r
          case f: Function0[AnyRef] => run(f())
          case (origin, fmap: Function1[ParseResult[AnyRef], AnyRef]) => run(origin match {
            case r: ParseResult[AnyRef] => fmap(r)
            case f: Function0[AnyRef] => (f(), fmap)
            case (o, f: Function1[ParseResult[AnyRef], AnyRef]) => (o, (result: ParseResult[AnyRef]) => (f(result), fmap))
          })
        }

      run(lazyParse(input)).asInstanceOf[ParseResult[S]]
    }

    override def >>[T](fn: S => Parser[T]): Parser[T] = {
      val flapMap = (result: ParseResult[S]) => result match {
        case Succ(r, n) => fn(r) match {
          case StateParser(lazyParser) => Cache(lazyParser, n) { () => lazyParser(n) }
          case p => p parse n
        }
        case f: Fail => f
      }

      val lazyParse = this.lazyParse
      StateParser[T] { input => (lazyParse(input), flapMap) }
    }

    override def |[T >: S](that: Parser[T]): Parser[T] = {
      val fn = that match {
        case StateParser(lazyParser) => Cache(lazyParser) {
          (input: Input) => Cache(lazyParser) { () => lazyParser(input) }
        }
        case p => (input) => p parse input
      }

      val lazyParse = this.lazyParse
      StateParser[T] { input =>
        Cache(fn) {
          (lazyParse(input), (result: ParseResult[S]) => result match {
            case s: Succ[S] => s
            case _ => fn(input)
          })
        }
      }
    }
  }

  /** zero unit */
  final def fail(msg: String): Parser[Nothing] = new Parser[Nothing] {
    override def parse(input: Input) = Fail(msg)
    override def >>[T](fn: Nothing => Parser[T]) = this
    override def |[T >: Nothing](that: Parser[T]): Parser[T] = that
    override def toString = s"fail($msg)"
  }

  /** unit */
  final def succ[S](sym: S): Parser[S] = new Parser[S] {
    override def parse(input: Input) = Succ(sym, input)
    override def >>[T](fn: S => Parser[T]) = fn(sym)
    override def |[T >: S](that: Parser[T]): Parser[T] = this
    override def toString = s"succ($sym)"
  }

  def p[T, S](self: T)(implicit fn: T => Parser[S]): Parser[S] = fn(self)
}