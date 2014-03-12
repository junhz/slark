package slark
package combinator.parser

import scala.annotation.tailrec

trait Parsers { parsers =>
  type Input

  sealed trait ParseResult[+S] {
  }

  case class Fail(msg: Any) extends ParseResult[Nothing]

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

  import Parsers._

  final def parser[S](fun: Input => ParseResult[S]): Parser[S] = CombinedParser { input => Done(fun(input)) }

  case class CombinedParser[S](fun: Input => Trampoline[ParseResult[S]]) extends Parser[S] {
    override def parse(input: Input) = fun(input).run

    override def >>[T](fn: S => Parser[T]): Parser[T] = {
      val flapMap = (result: ParseResult[S]) => result match {
        case Succ(r, n) => fn(r) match {
          case CombinedParser(fun) => Next1(fun, n)
          case p => Done(p parse n)
        }
        case f: Fail => Done(f)
      }

      Cache(fun)(CombinedParser { input => FlatMap(fun(input), flapMap) })
    }

    override def |[T >: S](that: Parser[T]): Parser[T] = Cache(fun) {
      that match {
        case CombinedParser(f) => Cache(f)(CombinedParser { input =>
          Cache(f) {
            FlatMap(fun(input), (result: ParseResult[S]) => result match {
              case s: Succ[S] => Done(s)
              case _ => Next1(f, input)
            })
          }
        })
        case p => Cache(p)(CombinedParser { input =>
          Cache(p) {
            FlatMap(fun(input), (result: ParseResult[S]) => result match {
              case s: Succ[S] => Done(s)
              case _ => Done(p parse input)
            })
          }
        })
      }
    }
  }

  /** zero unit */
  final def fail(msg: Any): Parser[Nothing] = new Parser[Nothing] {
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
  trait Trampoline[+T] {
    @tailrec
    final def run: T = {
      this match {
        case Done(r) => r
        case Next1(fun, in) => fun(in).run
        case FlatMap(pre, fun) => (pre match {
          case Done(r) => {
            fun(r)
          }
          case Next1(f, i) => FlatMap(f(i), fun)
          case FlatMap(p, f) => Cache(f, fun)(FlatMap(p, (result: Any) => FlatMap(f(result), fun)))
        }).run
      }
    }
  }
  case class Done[T](result: T) extends Trampoline[T]
  case class Next1[I, T](fun: I => Trampoline[T], in: I) extends Trampoline[T]
  case class FlatMap[T, P](pre: Trampoline[P], fun: P => Trampoline[T]) extends Trampoline[T] {
    override def toString = s"FlatMap($pre, ${fun.getClass()})"
  }
}