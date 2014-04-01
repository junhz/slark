package slark
package combinator.parser

import scala.annotation.tailrec
import Trampolines._

trait Parsers { parsers =>
  type Input
  
  type ^[+A, +B] = (A, B)
  final val ^ = Tuple2

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
    final def ^[T](that: Parser[T]): Parser[S ^ T] = self >> { x => that -> { y => (x, y) } }

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
      
    final def apply(min: Int, max: Int): Parser[List[S]] = 
      if(min > 0) self >> { x => self(min - 1, max - 1) -> { xs => x :: xs } } | fail("not enough")
      else if(max > 0) self >> { x => self(0, max - 1) -> { xs => x :: xs } } | succ(Nil)
      else succ(Nil)

    /** option */
    final def ? : Parser[Option[S]] = (self -> { x => Some(x) }) | succ(None)
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

  private[this] case class CombinedParser[S](fun: Input => Trampoline[ParseResult[S]]) extends Parser[S] {
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

  abstract class AbstractParser[S] extends Parser[S] {
    private[this] final lazy val self: Parser[S] = CombinedParser { input => Done(this parse input) }
    final override def >>[T](fn: S => Parser[T]): Parser[T] = self >> fn
    final override def |[T >: S](that: Parser[T]): Parser[T] = self | that
  }

  final def parser[S](fun: Input => ParseResult[S]): Parser[S] = CombinedParser { input => Done(fun(input)) }

  def p[T, S](self: T)(implicit fn: T => Parser[S]): Parser[S] = fn(self)
  
  val `>` = Int.MaxValue
  val `<` = 0
  val eof = Fail("reach the end of input")
}