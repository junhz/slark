package slark
package http

import combinator.parser._

trait Encoder {

  type CharParsers <: Parsers with Readers.Linear { type T = Char }
  type ByteParsers <: Parsers with Readers.Linear { type T = Byte }
  
  val charParsers: CharParsers
  val byteParsers: ByteParsers

  final class EncodedReader(val input: byteParsers.Input) extends charParsers.Reader {
    private[this] var hd: Char = _
    private[this] var tl: EncodedReader = _

    override lazy val atEnd = {
      if (input.atEnd) true
      else {
        val cnt = input.head
        if (cnt < 0) true
        else {
          hd = cnt.toChar
          tl = new EncodedReader(input.tail)
          false
        }
      }
    }

    override def head = if (atEnd) ??? else hd
    override def tail = if (atEnd) ??? else tl
  }

  final class EncodedParser[S](charParser: charParsers.Parser[S]) extends byteParsers.Parser[S] {
    override def parse(input: byteParsers.Input) = {
      charParser.parse(new EncodedReader(input)) match {
        case charParsers.Succ(r, n) => n match {
          case n: EncodedReader => byteParsers.Succ(r, n.input)
          case _ => byteParsers.Fail(CreateInputInParser)
        }
        case charParsers.Fail(msg) => byteParsers.Fail(msg:_*)
      }
    }
  }

  import scala.language.implicitConversions
  private[this] val singletonEncoder: (charParsers.Parser[Any] => byteParsers.Parser[Any]) = new EncodedParser(_)
  implicit def encode[S]: (charParsers.Parser[S] => byteParsers.Parser[S]) =
    singletonEncoder.asInstanceOf[charParsers.Parser[S] => byteParsers.Parser[S]]

  case object CreateInputInParser extends FailReason
  
}