package test

import slark.combinator.parser._
import slark.http._

package object myhttp {

  val acsiiParsers = new Parsers with slark.uri.CharReaders
  val byteParsers = new Parsers with OctetReaders with ImportChars[acsiiParsers.type] {
      protected[this] override def _charParsers = acsiiParsers
    }
  
  val symbols = new HttpSymbols[acsiiParsers.type, byteParsers.type] with DateTime { self =>
    protected[this] override def _parsers = byteParsers
    protected[this] override def _uriSymbols = new slark.uri.UriSymbols[parsers.charParsers.type] {
      protected[this] override def _parsers = self.parsers.charParsers
      protected[this] override def _name = "http"
      override def _port = 80
      protected[this] override def formatPath(path: List[String]) = path
    }
    protected[this] override def _options = new Options {
      override def rejectBWSAfterStartLine = true
      override def rejectBWSAfterHeaderFieldName = true
    }
  }

}