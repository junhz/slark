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

    def onSucc[T](fn: S => Parser[T]): Parser[T] = Cache(parsers) {
      Combinate(self,  (i: Input) => (r: Result[S]) => {
        println("onSucc")
        r match {
          case s: Succ[S] => (fn(s.result), s.next)
          case f: Fail  => (fail(f.msg), i)
        }
      })
    }

    def onFail[T >: S](fn: List[FailReason] => Parser[T]): Parser[T] = Cache(parsers) {
      Combinate(self, (i: Input) => (r: Result[S]) => {
        println("onFail")
        r match {
          case s: Succ[S] => (succ(s.result: T), s.next)
          case f: Fail  => (fn(f.msg), i)
        }
      })
    }

    def not: Parser[Unit] = Cache(parsers) {
      Combinate(self, (i: Input) => (r: Result[S]) => {
        println("not")
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
    def ->[T](fn: S => T): Parser[T] = self >> Cache(CombinatorParses.this) { x => succ(fn(x)) }

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

    override def bounce(t: (Parser[T], Input)) = { println(t); t._1 match {
      case Combinate(p, fn1) => p match {
        case Combinate(p, fn2) => (Combinate(p, associate(fn2, fn1)), t._2)
        case _ => (p parse t._2) match {
          case x @ Succ(r, n) => fn1(t._2)(x)
          case x @ Fail(msg)  => fn1(t._2)(x)
        }
      }
    }}

    def associate[A, B, C](fn1: Input => Result[A] => (Parser[B], Input),
                           fn2: Input => Result[B] => (Parser[C], Input)): Input => Result[A] => (Parser[C], Input) = {
      (i: Input) => (r: Result[A]) => {
          val (p, n) = fn1(i)(r)
          (Combinate(p, (input: Input) => fn2(i)), n)
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
  
  def eval(op: List[(Int, Char)], term: Int): Int = {
    @tailrec
    def _eval(nums: List[Int], ops: List[Char], result: Int): Int = {
      if (ops.isEmpty) result
      else ops.head match {
        case '+' => _eval(nums.tail, ops.tail, result + nums.head)
        case '-' => _eval(nums.tail, ops.tail, result - nums.head)
        case '*' => _eval(nums.tail, ops.tail, result * nums.head)
        case '/' => _eval(nums.tail, ops.tail, result / nums.head)
      }
    }
    if (op.isEmpty) term
    else {
      _eval(op.tail.map(_._1) ::: term :: Nil, op.map(_._2), op.head._1)
    }
  }
  
  val ws = ((' ': Parser[Char]) | '\t' | '\n' | '\r').*
  val dgt = ('0': Parser[Char]) | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
  val number = (dgt ^ dgt.*) -> (t => (t._1 :: t._2).foldLeft(0)((sum, ch) => sum * 10 + (ch - '0')))
  val term: Parser[Int] = (('(' :^ ws :^ ref(expr)) ^: ws ^: ')') | ('-' :^ ref(expr) -> (n => -n)) | number
  val factor: Parser[Int] = ((ws :^ ref(term) ^ ws :^ (('+': Parser[Char]) | '-' )).* ^ ws :^ ref(term)) -> (t => eval(t._1, t._2))
  val expr: Parser[Int] = ((ws :^ ref(factor) ^ (ws :^ ('*': Parser[Char]) | '/' )).* ^ ws :^ ref(factor)) -> (t => eval(t._1, t._2))
  
  val `1`: Parser[Char] = '1'
  val test = (`1`.!).*
  
  val calculator = (ws :^ expr) ^: ws
}