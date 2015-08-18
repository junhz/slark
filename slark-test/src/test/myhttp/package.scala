package test

import slark.combinator.parser._
import slark.http._

package object myhttp {

  val acsiiParsers = new Parsers with slark.uri.CharReaders
  val byteParsers = new Parsers with OctetReaders
  val httpUriSymbols = new { 
    val parsers: acsiiParsers.type = acsiiParsers
    val schemeName = "http"
    val defaultPort = 80
  } with slark.uri.UriSymbols {
    type P = acsiiParsers.type
    override def formatPath(path: List[String]) = path
  }
  
  val symbols = new {
    val parsers: byteParsers.type = byteParsers
    val uriSymbols: httpUriSymbols.type = httpUriSymbols
    val options = new Options {
      override def rejectBWSAfterStartLine = true
      override def rejectBWSAfterHeaderFieldName = true
    }
  } with HttpSymbols with DateTime {
    type P = byteParsers.type
    type UriSymbols = httpUriSymbols.type
  }

}