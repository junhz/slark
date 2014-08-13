package slark
package combinator.parser

import scala.annotation.tailrec
import Trampolines._
import scala.language.higherKinds

trait Parsers { parsers =>

  type Input

  sealed trait ParseResult[+S]
  
  trait FailApi extends ParseResult[Nothing]{
    def msg: List[FailReason]
  }
  
  type Fail <: FailApi
  
  trait FailExtractor {
    def apply(msg: FailReason*): Fail
    def unapply(f: Fail): Option[List[FailReason]]
  }
  
  val Fail: FailExtractor
  
  trait SuccApi[+S] extends ParseResult[S] {
    def result: S
    def next: Input
  }
  
  type Succ[+S] <: SuccApi[S]
  
  trait SuccExtractor {
    def apply[S](result: S, next: Input): Succ[S]
    def unapply[S](s: Succ[S]): Option[(S, Input)]
  }
  
  val Succ: SuccExtractor

  abstract class Parser[+S] { self =>
    type Result = ParseResult[S]

    def parse(input: Input): Result

    private[parser] def combined: Parser[S] = CombinedParser { input => Done(this parse input) }

    private[parser] def onSucc[T](fn: S => Parser[T]): Parser[T] = combined onSucc fn

    private[parser] def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = combined onFail fn

    private[parser] def not: Parser[Unit] = combined.not

    /** flatmap */
    final def >>[T](fn: S => Parser[T]): Parser[T] = this onSucc fn

    final def |>[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = this onFail fn

    /** plus */
    final def |[T >: S](that: Parser[T]): Parser[T] = this onFail { x => that }

    /** map */
    final def ->[T](fn: S => T): Parser[T] = self >> Cache(Parsers.this) { x => succ(fn(x)) }

    /** List */
    final def ^[T](that: Parser[T]): Parser[S ^ T] = self >> { x => that -> { y => (x, y) } }

    /** guard */
    final def :^[T](that: Parser[T]): Parser[T] = self >> { _ => that }

    /** guard */
    final def ^:[T](that: Parser[T]): Parser[T] = that >> { x => self -> { _ => x } }

    /** not */
    final def ! : Parser[Unit] = this.not

    /** rep */
    final def * : Parser[List[S]] = (self >> { x => self.* -> { xs => x :: xs } }) | succ(Nil)

    /** rep */
    final def apply(time: Int): Parser[List[S]] =
      if (time > 0) self >> { x => self { time - 1 } -> { xs => x :: xs } }
      else succ(Nil)

    final def apply(min: Int, max: Int): Parser[List[S]] =
      if (min > 0) self >> { x => self(min - 1, max - 1) -> { xs => x :: xs } } | fail(EOF)
      else if (max > 0) self >> { x => self(0, max - 1) -> { xs => x :: xs } } | succ(Nil)
      else succ(Nil)

    /** option */
    final def ? : Parser[Option[S]] = (self -> { x => Some(x) }) | succ(None)
  }

  /** zero unit */
  final def fail(msg: FailReason): Parser[Nothing] = new Parser[Nothing] {
    override def parse(input: Input) = Fail(msg)
    override def onSucc[T](fn: Nothing => Parser[T]) = this
    override def onFail[T >: Nothing](fn: List[FailReason] => Parser[T]) = fn(msg :: Nil)
    override def not = succ(())
    override def toString = s"fail($msg)"
  }

  /** unit */
  final def succ[S](sym: S): Parser[S] = new Parser[S] {
    override def parse(input: Input) = Succ(sym, input)
    override def onSucc[T](fn: S => Parser[T]) = fn(sym)
    override def onFail[T >: S](that: List[FailReason] => Parser[T]) = this
    override def not = fail(MissingExpectedFailure)
    override def toString = s"succ($sym)"
  }

  private[this] case class CombinedParser[S](fun: Input => Trampoline[ParseResult[S]]) extends Parser[S] {
    override def parse(input: Input) = fun(input).run

    override def onSucc[T](fn: S => Parser[T]): Parser[T] = {
      val associated = (_: ParseResult[S]) match {
        case Succ(r, n) => fn(r) match {
          case CombinedParser(fun) => Next1(fun, n)
          case p => Done(p parse n)
        }
        case f: Fail => Done(f)
      }

      Cache(fun)(CombinedParser { input => FlatMap(fun(input), associated) })
    }

    override def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = Cache(fun) {
      CombinedParser { input =>
        Cache(fn) {
          val associated = (_: ParseResult[S]) match {
            case s: Succ[S] => Done(s)
            case Fail(msg) => fn(msg) match {
              case CombinedParser(f) => Next1(f, input)
              case p => Done(p parse input)
            }
          }
          FlatMap(fun(input), associated)
        }
      }
    }

    override def not: Parser[Unit] = Cache(fun, Parsers.this) {
      CombinedParser { input =>
        Cache(Parsers.this) {
          FlatMap(fun(input), (_: ParseResult[S]) match {
            case s: Succ[S] => Done(Fail(MissingExpectedFailure))
            case _ => Done(Succ((), input))
          })
        }

      }
    }
  }

  final def parser[S](fun: Input => ParseResult[S]): Parser[S] = CombinedParser { input => Done(fun(input)) }

  final def p[T, S](self: T)(implicit fn: T => Parser[S]): Parser[S] = fn(self)

  final def `>` = Int.MaxValue
  final def `<` = 0
  final def eof = Fail(EOF)

  val EOF: FailReason
  val MissingExpectedFailure: FailReason
}