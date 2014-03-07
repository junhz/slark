package slark
package combinator.parser

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
    final def ->[T](fn: S => T): Parser[T] = self >> Slim { x => succ(fn(x)) }

    /** seq */
    final def ^[T](that: Parser[T]): Parser[(S, T)] = Parser ^ (this, that)

    /** guard */
    final def :^[T](that: Parser[T]): Parser[T] = Parser :^ (this, that)

    /** guard */
    final def ^:[T](that: Parser[T]): Parser[T] = Parser.^:(this, that)

    /** not */
    final def ! : Parser[Unit] = Parser ! this

    /** rep */
    final def * : Parser[List[S]] = Parser * this

    /** rep */
    final def apply(time: Int): Parser[List[S]] = Parser(this, time)

    /** option */
    final def ? : Parser[Option[S]] = Parser ? this

    /** option rep */
    /*final def ?(time: Int): Parser[List[S]] =
      if (time > 0) (self >> { x => self.?{ time - 1 } -> { xs => x :: xs } }) | succ(Nil)
      else throw new IllegalArgumentException("repeat time should  be greater then 0")*/
  }
  object Parser {
    //def ->[S, T](self: Parser[S], fn: S => T): Parser[T] = self >> { x => succ(fn(x)) }

    /** seq */
    def ^[S, T](self: Parser[S], that: Parser[T]): Parser[(S, T)] = self >> { x => that -> { y => (x, y) } }

    /** guard */
    def :^[S, T](self: Parser[S], that: Parser[T]): Parser[T] = self >> { _ => that }

    /** guard */
    def ^:[S, T](self: Parser[S], that: Parser[T]): Parser[T] = that >> { x => self -> { _ => x } }

    /** not */
    def ![S](self: Parser[S]): Parser[Unit] = ((self >> { _ => succ(fail("missing an expected failure")) }) | succ(succ())) >> { p => p }

    /** rep */
    def *[S](self: Parser[S]): Parser[List[S]] = (self >> { x => self.* -> { xs => x :: xs } }) | succ(Nil)

    /** rep */
    def apply[S](self: Parser[S], time: Int): Parser[List[S]] =
      if (time > 0) self >> { x => self { time - 1 } -> { xs => x :: xs } }
      else succ(Nil)

    /** option */
    final def ?[S](self: Parser[S]): Parser[Option[S]] = (self >> { x => succ(Some(x)) }) | succ(None)
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
    override def >>[T](fn: S => Parser[T]): Parser[T] = StateParser >> (this, fn)

    override def |[T >: S](that: Parser[T]): Parser[T] = StateParser | (this, that)
    
    def l(input: Input): Function0[AnyRef] =new StateParser.L(lazyParse, input)
  }
  object StateParser {
    final class L[T](lazyParser: Input => T, input: Input) extends Function0[T] {
      override def apply = lazyParser(input)
    }
    def >>[S, T](self: StateParser[S], fn: S => Parser[T]): Parser[T] = {
      val fmap = (result: ParseResult[S]) => result match {
        case Succ(r, n) => fn(r) match {
          case s: StateParser[_] => s.l(n)
          case p => p parse n
        }
        case f: Fail => f
      }

      StateParser[T](Trampoline >> (self.lazyParse, _ => fmap))
    }

    def |[S, T >: S](self: StateParser[S], that: Parser[T]): Parser[T] = {
      val fn = that match {
        case s: StateParser[_] => (input: Input) =>s.l(input)
        case p => (input: Input) => p parse input
      }
      
      val fmap = (input: Input) => (result: ParseResult[S]) => result match {
        case s: Succ[S] => s
        case _ => fn(input)
      }

      StateParser[T](Trampoline >> (self.lazyParse, fmap))
    }
  }
  object Trampoline {
    def >>[S, T](self: S => AnyRef, fn: S => (T => AnyRef)): S => AnyRef =
      s => (self(s), fn(s))
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