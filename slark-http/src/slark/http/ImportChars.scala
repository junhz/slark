package slark
package http

import combinator.parser._

trait ImportChars[P <: Parsers with Readers[Char]] { self: Parsers with Readers[Byte] =>

  val charParsers: P

  final class TransedCharReader(val input: Input) extends charParsers.Reader {
    private[this] var hd: charParsers.From = _
    private[this] var tl: TransedCharReader = _

    override lazy val atEnd = {
      if (input.atEnd) true
      else {
        val cnt = input.head
        if (cnt < 0) true
        else {
          hd = cnt.toChar
          tl = new TransedCharReader(input.tail)
          false
        }
      }
    }

    override def head = if (atEnd) ??? else hd
    override def tail = if (atEnd) ??? else tl
  }

  final class TransParser[S](uriParser: charParsers.Parser[S]) extends AbstractParser[S] {
    override def parse(input: Input) = {
      uriParser.parse(new TransedCharReader(input)) match {
        case charParsers.Succ(r, n) => n match {
          case n: TransedCharReader => Succ(r, n.input)
          case _ => Fail("parser shouldn't create input")
        }
        case charParsers.Fail(msg) => Fail(msg)
      }
    }
  }

  import scala.language.implicitConversions
  private[this] val singletonUriParserToCntParser: (charParsers.Parser[Any] => Parser[Any]) = new TransParser(_)
  implicit def uriParserToCntParser[S]: (charParsers.Parser[S] => Parser[S]) =
    singletonUriParserToCntParser.asInstanceOf[charParsers.Parser[S] => Parser[S]]

}