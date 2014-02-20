package slark.combinator.parser

import scala.annotation.tailrec

trait Parsers {
  type Input

  sealed trait ParseResult[+S] {
  }

  case class Fail(msg: String) extends ParseResult[Nothing]

  case class Succ[+S](result: S, next: Input) extends ParseResult[S]

  sealed trait Parser[+S] { self =>
    type Result = ParseResult[S]

    def parse(input: Input): Result

    /** flatmap */
    def >>[T](fn: S => Parser[T]): Parser[T]

    /** plus */
    def |[T >: S](that: Parser[T]): Parser[T]

    /** map */
    final def ->[T](fn: S => T): Parser[T] = self >> { x => succ(fn(x)) }

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

  trait Trampoline[+S] {
    @tailrec
    final def resume: Trampoline[S] =
      this match {
        case FlatMap(t, fn) => t match {
          case Strict(r) => fn(r).resume
          case Lazy(input, lazyParse) => FlatMap(lazyParse(input), fn)
          case FlatMap(t_, fn_) => (t_ flatmap { result => fn_(result) flatmap fn }).resume
        }
        case t => t
      }
    @tailrec
    final def run: ParseResult[S] = resume match {
      case Strict(r) => r
      case t => t.run
    }
    final def flatmap[T](fn: ParseResult[S] => Trampoline[T]): Trampoline[T] = this match {
      case FlatMap(t, fn_) => FlatMap(t, (result: ParseResult[Any]) => fn_(result) flatmap fn)
      case t => FlatMap(t, fn)
    }
  }
  case class Strict[+S](result: ParseResult[S]) extends Trampoline[S]
  case class Lazy[+S](input: Input, lazyParse: Input => Trampoline[S]) extends Trampoline[S]
  case class FlatMap[S, +T](sub: Trampoline[S], fn: ParseResult[S] => Trampoline[T]) extends Trampoline[T]

  final def parser[S](fn: Input => ParseResult[S]): Parser[S] = StateParser(input => Strict(fn(input)))

  case class StateParser[S](lazyParse: Input => Trampoline[S]) extends Parser[S] {
    override def parse(input: Input) = lazyParse(input).run
    override def >>[T](fn: S => Parser[T]): Parser[T] =
      StateParser[T] { input =>
        lazyParse(input) flatmap {
          _ match {
            case Succ(r, n) => fn(r) match {
              case StateParser(lazyParse) => Lazy(n, lazyParse)
              case p => Strict(p parse n)
            }
            case f: Fail => Strict(f)
          }
        }
      }
    override def |[T >: S](that: Parser[T]): Parser[T] =
      StateParser[T] { input =>
        lazyParse(input) flatmap {
          _ match {
            case s: Succ[S] => Strict(s)
            case f: Fail => that match {
              case StateParser(lazyParse) => Lazy(input, lazyParse)
              case p => Strict(p parse input)
            }
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
  }

  def p[T, S](self: T)(implicit fn: T => Parser[S]): Parser[S] = fn(self)
}