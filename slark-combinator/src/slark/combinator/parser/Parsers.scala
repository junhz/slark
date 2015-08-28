package slark
package combinator.parser

import scala.annotation.tailrec
import Trampolines._
import scala.language.higherKinds

abstract class Parsers extends ParsersApi { parsers =>
  
  case class Fail(val msg: List[FailReason]) extends FailApi
  object Fail extends FailExtractor
  
  case class Succ[+S](val result: S, val next: Input) extends SuccApi[S]
  object Succ extends SuccExtractor

  abstract class Parser[+S] extends ParserApi[S] { self =>

    def parse(input: Input): ParseResult[S]

    def onSucc[T](fn: S => Parser[T]): Parser[T] = Combinate(self, Cache(parsers) ((r: ParseResult[S], i: Input) => {
      r match {
        case s: Succ[S] => (fn(s.result), s.next)
        case f: Fail  => (fail(f.msg), i)
      }
    }))

    def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = Combinate(self, Cache(parsers) ((r: ParseResult[S], i: Input) => {
      r match {
        case s: Succ[S] => (succ(s.result: T), s.next)
        case f: Fail  => (fn(f.msg), i)
      }
    }))

    def not: Parser[Unit] = Combinate(self, Cache(parsers) ((r: ParseResult[S], i: Input) => {
      r match {
        case _: Succ[S] => (fail(MissingExpectedFailure :: Nil), i)
        case f: Fail  => (succ(()), i)
      }
    }))

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
      if (min > 0) self >> { x => self(min - 1, max - 1) -> { xs => x :: xs } } | fail(EOF :: Nil)
      else if (max > 0) self >> { x => self(0, max - 1) -> { xs => x :: xs } } | succ(Nil)
      else succ(Nil)

    /** option */
    def ? : Parser[Option[S]] = (self -> { x => Some(x) }) | succ(None)
  }

  /** zero unit */
  def fail(msg: List[FailReason]): Parser[Nothing] = new Parser[Nothing] {
    override def parse(input: Input) = Fail(msg)
    override def onSucc[T](fn: Nothing => Parser[T]) = this
    override def onFail[T >: Nothing](fn: List[FailReason] => Parser[T]) = fn(msg)
    override def not = succ(())
    override def toString = s"fail($msg)"
  }

  /** unit */
  def succ[S](sym: S): Parser[S] = new Parser[S] {
    override def parse(input: Input) = Succ(sym, input)
    override def onSucc[T](fn: S => Parser[T]) = fn(sym)
    override def onFail[T >: S](that: List[FailReason] => Parser[T]) = this
    override def not = fail(MissingExpectedFailure :: Nil)
    override def toString = s"succ($sym)"
  }

  case class Combinate[S, T](p: Parser[S], fn: (ParseResult[S], Input) => (Parser[T], Input)) extends Parser[T] { combinate =>
    override def parse(input: Input) = {
      val (p, i) = parserAndInputIsTrampoline.run((combinate, input))
      p parse i
    }
  }

  def parserAndInputIsTrampoline[T]: IsTrampoline[(Parser[T], Input)] = new IsTrampoline[(Parser[T], Input)] {
    override def isDone(t: (Parser[T], Input)) = t._1 match {
      case _: Combinate[_, _] => false
      case _                  => true
    }

    override def bounce(t: (Parser[T], Input)) = t._1 match {
      case Combinate(p, fn1) => p match {
        case Combinate(p, fn2) => (Combinate(p, associate(fn2, fn1)), t._2)
        case _ => (p parse t._2) match {
          case x @ Succ(r, n) => fn1(x, n)
          case x @ Fail(msg)  => fn1(x, t._2)
        }
      }
    }

    def associate[A, B, C](fn1: (ParseResult[A], Input) => (Parser[B], Input),
                           fn2: (ParseResult[B], Input) => (Parser[C], Input)): (ParseResult[A], Input) => (Parser[C], Input) = {
      (r: ParseResult[A], i: Input) => {
          val (p, n) = fn1(r, i)
          (Combinate(p, fn2), n)
        }
    }
  }

  val `>` = Int.MaxValue
  
  val `<` = 0

  val EOF: FailReason = FailReason("end of input")
  
  val eof = Fail(EOF :: Nil)
  
  val MissingExpectedFailure: FailReason = FailReason("missing an expected failure")
}