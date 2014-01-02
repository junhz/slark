package slark
package http

import parser._
import uri._
import uri.{ Literals => UriLiterals }

trait UriApi { self: Symbols[Parsers] =>

  val uriSymbols = new Scheme.AbstractScheme("http", 80, new CombinatorParsers with ReaderApi with CharReader with Formats) {
    override def formatPath(path: List[String]) = path
  }

  def trans1(input: self.parsers.Input): Option[(uriSymbols.parsers.From, self.parsers.Input)]

  final class TransedCharReader(val input: self.parsers.Input) extends uriSymbols.parsers.CharReader {
    private[this] var hd: uriSymbols.parsers.From = _
    private[this] var tl: TransedCharReader = _

    override lazy val atEnd = {
      trans1(input) match {
        case None => true
        case Some((h, t)) => {
          hd = h
          tl = new TransedCharReader(t)
          false
        }
      }
    }

    override def head = if (atEnd) ??? else hd
    override def tail = if (atEnd) ??? else tl
  }

  final class TransParser[S](uriParser: uriSymbols.parsers.Parser[S]) extends self.parsers.Parser[S] {
    override def parse(input: self.parsers.Input) = {
      uriParser.parse(new TransedCharReader(input)) match {
        case uriSymbols.parsers.Succ(r, n) => n match {
          case n: TransedCharReader => self.parsers.Succ(r, n.input)
          case _ => self.parsers.Fail("parser shouldn't create input")
        }
        case uriSymbols.parsers.Fail(msg) => self.parsers.Fail(msg)
      }
    }
  }

  import scala.language.implicitConversions
  private[this] val singletonUriParserToCntParser: (uriSymbols.parsers.Parser[Any] => self.parsers.Parser[Any]) = new TransParser(_)
  implicit def uriParserToCntParser[S]: (uriSymbols.parsers.Parser[S] => self.parsers.Parser[S]) =
    singletonUriParserToCntParser.asInstanceOf[uriSymbols.parsers.Parser[S] => self.parsers.Parser[S]]
}