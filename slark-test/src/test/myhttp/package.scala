package test

import slark.combinator.parser._
import slark.http._

package object myhttp {

  val symbols = new HttpSymbols[Parsers with OctetReaders with ImportChars[Parsers with slark.uri.CharReaders]] with DateTime { self =>
    protected[this] override def _parsers = new Parsers with OctetReaders with ImportChars[Parsers with slark.uri.CharReaders] {
      protected[this] override def _charParsers = new Parsers with slark.uri.CharReaders
    }
    protected[this] override def _uriSymbols = new slark.uri.UriSymbols[parsers.charParsers.type] {
      protected[this] override def _parsers = self.parsers.charParsers
      protected[this] override def _name = "http"
      protected[this] override def _port = 80
      protected[this] override def formatPath(path: List[String]) = path
    }
    protected[this] override def _options = new Options {
      override def rejectBWSAfterStartLine = true
      override def rejectBWSAfterHeaderFieldName = true
    }
  }

}