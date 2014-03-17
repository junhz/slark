package test

import slark.combinator.parser._
import slark.http._

package object myhttp {

  object MyHttp extends Message.AbstractMessage(
    new Parsers with OctetReaders with ImportChars[Parsers with slark.uri.CharReaders] {
      override val charParsers = new Parsers with slark.uri.CharReaders
    }) with DateTime {
    override val uriSymbols = new slark.uri.Scheme.AbstractScheme[parsers.charParsers.type]("http", 80, parsers.charParsers) {
      override def formatPath(path: List[String]) = path
    }
  }

}