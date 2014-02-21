package slark.combinator.parser

import scala.annotation.tailrec

trait Parsers {
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

  sealed abstract class Trampoline[+S] {
  }
  final class Strict[+S](val result: ParseResult[S]) extends Trampoline[S]
  def strict[S](result: ParseResult[S]) = new Strict(result)

  final class NonStrict[+S](input: Input, next: Input => Trampoline[S]) extends Trampoline[S] {
    def toStrict: Trampoline[S] = next(input)
  }
  def nonStrict[S](input: Input, next: Input => Trampoline[S]): Trampoline[S] = new NonStrict(input, next)

  final class Combined[S, +T](val origin: Trampoline[S], val fmap: ParseResult[S] => Trampoline[T]) extends Trampoline[T]
  def combine[S, T](origin: Trampoline[S])(fmap: ParseResult[S] => Trampoline[T]): Trampoline[T] = new Combined(origin, fmap)

  final def parser[S](fn: Input => ParseResult[S]): Parser[S] = StateParser(input => strict(fn(input)))

  case class StateParser[S](lazyParse: Input => Trampoline[S]) extends Parser[S] {
    override def parse(input: Input) = {
      @tailrec
      def run[S](t: Trampoline[S]): ParseResult[S] = t match {
        case s: Strict[S] => s.result
        case n: NonStrict[S] => run(n.toStrict)
        case c: Combined[_, S] => run(c.origin match {
          case s: Strict[_] => c.fmap(s.result)
          case n: NonStrict[_] => combine(n.toStrict)(c.fmap)
          case cc: Combined[_, _] => combine(cc.origin) { result => combine(cc.fmap(result))(c.fmap) }
        })
      }
      run(lazyParse(input))
    }
    override def >>[T](fn: S => Parser[T]): Parser[T] =
      StateParser[T] { input =>
        combine(lazyParse(input)) {
          _ match {
            case Succ(r, n) => fn(r) match {
              case StateParser(lazyParse) => nonStrict(n, lazyParse)
              case p => strict(p parse n)
            }
            case f: Fail => strict(f)
          }
        }
      }
    override def |[T >: S](that: Parser[T]): Parser[T] =
      StateParser[T] { input =>
        combine(lazyParse(input)) {
          _ match {
            case s: Succ[S] => strict(s)
            case f: Fail => that match {
              case StateParser(lazyParse) => nonStrict(input, lazyParse)
              case p => strict(p parse input)
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