package slark
package calculator

import Trampolines._

/**
 * @author a554114
 */
class CombinatorParses { parsers =>
  trait Result[+T]
  
  case class Fail(val msg: List[FailReason]) extends Result[Nothing]
  
  case class Succ[+S](val result: S, val next: Input) extends Result[S]
  
  abstract class Parser[+S] { self =>

    def parse(input: Input): Result[S]

    def onSucc[T](fn: S => Parser[T]): Parser[T] = {
      Combinate(self,  (i: Input) => (r: Result[S]) => {
        //println("onSucc")
        r match {
          case s: Succ[S] => (fn(s.result), s.next)
          case f: Fail  => (fail(f.msg), i)
        }
      })
    }

    def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = {
      Combinate(self, (i: Input) => (r: Result[S]) => {
        //println("onFail")
        r match {
          case s: Succ[S] => (succ(s.result: T), s.next)
          case f: Fail  => (fn(f.msg), i)
        }
      })
    }

    def not: Parser[Unit] = {
      Combinate(self, (i: Input) => (r: Result[S]) => {
        //println("not")
        r match {
          case _: Succ[S] => (fail(MissingExpectedFailure), i)
          case f: Fail  => (succ(()), i)
        }
      })
    }

    /** flatmap */
    def >>[T](fn: S => Parser[T]): Parser[T] = this onSucc fn

    def |>[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = this onFail fn

    /** plus */
    def |[T >: S](that: Parser[T]): Parser[T] = this onFail { x => that }

    /** map */
    def ->[T](fn: S => T): Parser[T] = self >> { x => succ(fn(x)) }

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
  def fail(msg: List[FailReason]): Parser[Nothing] = new Parser[Nothing] {
    override def parse(input: Input) = Fail(msg)
    override def onSucc[T](fn: Nothing => Parser[T]) = this
    override def onFail[T >: Nothing](fn: List[FailReason] => Parser[T]) = fn(msg)
    override def not = succ(())
    override def toString = s"fail($msg)"
  }
  
  def fail(msg: FailReason): Parser[Nothing] = fail(msg :: Nil)

  /** unit */
  def succ[S](sym: S): Parser[S] = new Parser[S] {
    override def parse(input: Input) = Succ(sym, input)
    override def onSucc[T](fn: S => Parser[T]) = fn(sym)
    override def onFail[T >: S](that: List[FailReason] => Parser[T]) = this
    override def not = fail(MissingExpectedFailure)
    override def toString = s"succ($sym)"
  }

  case class Combinate[S, T](p: Parser[S], fn: Input => Result[S] => (Parser[T], Input)) extends Parser[T] { combinate =>
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

    override def bounce(t: (Parser[T], Input)) = {
      //println(t)
      t._1 match {
        case Combinate(p, fn1) => p match {
          case Combinate(p, fn2) => (Combinate(p, _ => associate(fn2(t._2), fn1(t._2))), t._2)
          case _ => fn1(t._2)(p parse t._2)
        }
      }}

    def associate[A, B, C](fn1: Result[A] => (Parser[B], Input),
                           fn2: Result[B] => (Parser[C], Input)): Result[A] => (Parser[C], Input) = {
      (r: Result[A]) => {
          val (p, n) = fn1(r)
          (Combinate(p, _ => fn2), n)
        }
    }
  }

  val EOF: FailReason = FailReason("end of input")
  
  val MissingExpectedFailure: FailReason = FailReason("missing an expected failure")
  
  implicit class CharParser(c: Char) extends Parser[Char] {
    def parse(input: Input): Result[Char] = {
      if (input.atEnd) {
        Fail(EOF :: Nil)
      } else if (input.cnt == c) {
        Succ(c, input.next)
      } else {
        Fail(FailReason(s"expect $c got ${input.cnt}") :: Nil)
      }
    }
  }
  
  def ref[T](parser: => Parser[T]): Parser[T] = Combinate(succ(()), (input: Input) => (unit: Result[Unit]) => (parser, input))
  
  @tailrec
  final def eval(result: Int, op: List[(Char, Int)]): Int = {
    if (op.isEmpty) result
    else op.head._1 match {
      case '+' => eval(result + op.head._2, op.tail)
      case '-' => eval(result - op.head._2, op.tail)
      case '*' => eval(result * op.head._2, op.tail)
      case '/' => eval(result / op.head._2, op.tail)
    }
    
  }
  
  val ws = ((' ': Parser[Char]) | '\t' | '\n' | '\r').*
  val dgt = ('0': Parser[Char]) | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
  val number = (dgt ^ dgt.*) -> (t => (t._1 :: t._2).foldLeft(0)((sum, ch) => sum * 10 + (ch - '0')))
  val term: Parser[Int] = (('(' :^ ws :^ ref(expr)) ^: ws ^: ')') | number
  val factor: Parser[Int] = (ws :^ term ^ (ws :^ (('*': Parser[Char]) | '/' ) ^ ws :^ term).*) -> (t => eval(t._1, t._2))
  val expr: Parser[Int] = (ws :^ factor ^ (ws :^ (('+': Parser[Char]) | '-' ) ^ ws :^ factor).*) -> (t => eval(t._1, t._2))
  
  val calculator = (ws :^ expr) ^: ws
}