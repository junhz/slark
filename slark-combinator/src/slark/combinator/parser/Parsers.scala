package slark
package combinator.parser

import scala.annotation.tailrec
import Trampolines._
import scala.language.higherKinds

abstract class Parsers extends ParsersApi { parsers =>
  
  final class Fail(val msg: List[FailReason]) extends FailApi
  
  val Fail = new FailExtractor {
    def apply(msg: FailReason*) = new Fail(List(msg:_*))
    def unapply(f: Fail) = Some(f.msg)
  }
  
  final class Succ[+S](val result: S, val next: Input) extends SuccApi[S]
  
  val Succ = new SuccExtractor {
    def apply[S](result: S, next: Input) = new Succ(result, next)
    def unapply[S](s: Succ[S]) = Some((s.result, s.next))
  }

  abstract class Parser[+S] extends ParserApi[S] { self =>

    def parse(input: Input): Result

    def combined: Parser[S] = new CombinedParser(input => Done(this parse input))

    def onSucc[T](fn: S => Parser[T]): Parser[T] = combined onSucc fn

    def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = combined onFail fn

    def not: Parser[Unit] = combined.not

    /** flatmap */
    def >>[T](fn: S => Parser[T]): Parser[T] = this onSucc fn

    def |>[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = this onFail fn

    /** plus */
    def |[T >: S](that: Parser[T]): Parser[T] = this onFail { x => that }

    /** map */
    def ->[T](fn: S => T): Parser[T] = self >> Cache(Parsers.this) { x => succ(fn(x)) }

    /** List */
    def ^[T](that: Parser[T]): Parser[S ^ T] = self >> { x => that -> { y => (x, y) } }

    /** guard */
    def :^[T](that: Parser[T]): Parser[T] = self >> { _ => that }

    /** guard */
    def ^:[T](that: Parser[T]): Parser[T] = that >> { x => self -> { _ => x } }

    /** not */
    def ! : Parser[Unit] = this.not

    /** rep */
    def * : Parser[List[S]] = (self >> { x => self.* -> { xs => x :: xs } }) | succ(Nil)

    /** rep */
    def apply(time: Int): Parser[List[S]] =
      if (time > 0) self >> { x => self { time - 1 } -> { xs => x :: xs } }
      else succ(Nil)

    def apply(min: Int, max: Int): Parser[List[S]] =
      if (min > 0) self >> { x => self(min - 1, max - 1) -> { xs => x :: xs } } | fail(EOF)
      else if (max > 0) self >> { x => self(0, max - 1) -> { xs => x :: xs } } | succ(Nil)
      else succ(Nil)

    /** option */
    def ? : Parser[Option[S]] = (self -> { x => Some(x) }) | succ(None)
  }

  /** zero unit */
  def fail(msg: FailReason): Parser[Nothing] = new Parser[Nothing] {
    override def parse(input: Input) = Fail(msg)
    override def onSucc[T](fn: Nothing => Parser[T]) = this
    override def onFail[T >: Nothing](fn: List[FailReason] => Parser[T]) = fn(msg :: Nil)
    override def not = succ(())
    override def toString = s"fail($msg)"
  }

  /** unit */
  def succ[S](sym: S): Parser[S] = new Parser[S] {
    override def parse(input: Input) = Succ(sym, input)
    override def onSucc[T](fn: S => Parser[T]) = fn(sym)
    override def onFail[T >: S](that: List[FailReason] => Parser[T]) = this
    override def not = fail(MissingExpectedFailure)
    override def toString = s"succ($sym)"
  }

  final class CombinedParser[S](val fun: Input => Trampoline[ParseResult[S]]) extends Parser[S] {
    def parse(input: Input) = fun(input).run

    override def onSucc[T](fn: S => Parser[T]): Parser[T] = {
      val associated = (_: ParseResult[S]) match {
        case Succ(r, n) => fn(r) match {
          case cp: CombinedParser[T] => Next1(cp.fun, n)
          case p => Done(p parse n)
        }
        case f: Fail => Done(f)
      }

      Cache(fun)(new CombinedParser(input => FlatMap(fun(input), associated)))
    }

    override def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = Cache(fun) {
      new CombinedParser(input =>
        Cache(fn) {
          val associated = (_: ParseResult[S]) match {
            case s: Succ[S] => Done(s)
            case Fail(msg) => fn(msg) match {
              case cp: CombinedParser[T] => Next1(cp.fun, input)
              case p => Done(p parse input)
            }
          }
          FlatMap(fun(input), associated)
        })
    }

    override def not: Parser[Unit] = Cache(fun, Parsers.this) {
      new CombinedParser(input =>
        Cache(Parsers.this) {
          FlatMap(fun(input), (_: ParseResult[S]) match {
            case s: Succ[S] => Done(Fail(MissingExpectedFailure))
            case _ => Done(Succ((), input))
          })
        })
    }
  }

  def parser[S](fun: Input => ParseResult[S]): Parser[S] = new CombinedParser(input => Done(fun(input)))

  def p[T, S](self: T)(implicit fn: T => Parser[S]): Parser[S] = fn(self)

  val `>` = Int.MaxValue
  
  val `<` = 0
  
  val eof = Fail(EOF)

  val EOF: FailReason = FailReason("end of input")
  
  val MissingExpectedFailure: FailReason = FailReason("missing an expected failure")
}