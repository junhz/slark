package slark
package http

import parser._

trait ListOfAst { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with OctetReader] with Literals =>

  import parsers._

  final class ListParser[S](p: Parser[S], min: Int) extends Parser[List[S]] {

    private[this] val leading = ows :^ p
    private[this] val tail = (ows ^ "," ^ ows) :^ p

    override def parse(input: Input) = {
      def rec(next: Input, result: List[S], count: Int): ParseResult[List[S]] = {
        tail.parse(next) match {
          case Succ(r, n) => rec(n, r :: result, count + 1)
          case Fail(msg) => if (count < min) Fail(s"failed at ${count + 1} attempt: $msg") else Succ(result.reverse, next)
        }
      }

      leading.parse(input) match {
        case Succ(r, n) => rec(n, List(r), 1)
        case _ => Succ(Nil, input)
      }
    }
  }

  implicit class ListOf[T](self: T) {
    def `#`[S](implicit fn: T => Parser[S]): ListParser[S] = new ListParser[S](fn(self), 0)
  }

  implicit class ListOfSome[S](repeated: Repeated[S]) {
    def `#` = new ListParser[S](repeated.parser, repeated.count)
  }

}