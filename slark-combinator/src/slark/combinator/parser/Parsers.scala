package slark.combinator.parser

import scala.annotation.tailrec

trait Parsers { parsers =>
  type Input

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
    final def ->[T](fn: S => T): Parser[T] = self >> Parsers.->(parsers)(fn)

    /** seq */
    final def ^[T](that: Parser[T]): Parser[(S, T)] = self >> { x => that -> { y => (x, y) } }

    /** guard */
    final def :^[T](that: Parser[T]): Parser[T] = self >> { _ => that }

    /** guard */
    final def ^:[T](that: Parser[T]): Parser[T] = that >> { x => self -> { _ => x } }

    /** not */
    final def ! : Parser[Unit] = ((self >> { _ => succ(fail("missing an expected failure")) }) | succ(succ())) >> { p => p }

    /** rep */
    final def * : Parser[List[S]] = (self >> { x => self.* -> { xs => x :: xs } }) | succ(Nil)

    /** rep */
    final def apply(time: Int): Parser[List[S]] =
      if (time > 0) self >> { x => self { time - 1 } -> { xs => x :: xs } }
      else succ(Nil)

    /** option */
    final def ? : Parser[Option[S]] = (self >> { x => succ(Some(x)) }) | succ(None)

    /** option rep */
    /*final def ?(time: Int): Parser[List[S]] =
      if (time > 0) (self >> { x => self.?{ time - 1 } -> { xs => x :: xs } }) | succ(Nil)
      else throw new IllegalArgumentException("repeat time should  be greater then 0")*/
  }

  final def parser[S](fn: Input => ParseResult[S]): Parser[S] = StateParser(input => fn(input))

  case class StateParser[S](lazyParse: Input => AnyRef) extends Parser[S] {
    override def parse(input: Input) = {
        var i = 0
      @tailrec
      def run(a: AnyRef): AnyRef = {
          i = i + 1
          println(i)
        a match {
          case r: ParseResult[AnyRef] => r
          case f: Function0[AnyRef] => run(f())
          case (origin, fmap: Function1[ParseResult[AnyRef], AnyRef]) => run(origin match {
            case r: ParseResult[AnyRef] => fmap(r)
            case f: Function0[AnyRef] => (f(), fmap)
            case (o, f: Function1[ParseResult[AnyRef], AnyRef]) => (o, (result: ParseResult[AnyRef]) => (f(result), fmap))
          })
        }
      }
      run(lazyParse(input)).asInstanceOf[ParseResult[S]]
    }
    override def >>[T](fn: S => Parser[T]): Parser[T] = {
      StateParser[T](Parsers.>>(parsers)(lazyParse, fn))
    }
    override def |[T >: S](that: Parser[T]): Parser[T] =
      StateParser[T] { input =>
        (lazyParse(input), (result: ParseResult[S]) => result match {
          case s: Succ[S] => s
          case f: Fail => that match {
            case StateParser(lazyParse) => () => lazyParse(input)
            case p => p parse input
          }
        })
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

object Parsers {
  def ->[S, T](parsers: Parsers)(fn: S => T): S => parsers.Parser[T] = x => parsers.succ(fn(x))
  def >>[S, T](parsers: Parsers)(lazyParse: parsers.Input => AnyRef, fn: S => parsers.Parser[T]): parsers.Input => AnyRef =
    input => () => (lazyParse(input), (result: parsers.ParseResult[S]) => result match {
      case parsers.Succ(r, n) => fn(r) match {
        case parsers.StateParser(lazyParse) => () => lazyParse(n)
        case p => p parse n
      }
      case f: parsers.Fail => f
    })
}