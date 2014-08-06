package test

import slark.combinator.parser._
import slark.http._

package object myhttp {

  val acsiiParsers = new Parsers with slark.uri.CharReaders
  val byteParsers = new Parsers with OctetReaders
  val httpUriSymbols = new { 
    type P = acsiiParsers.type
    val parsers: acsiiParsers.type = acsiiParsers
    val schemeName = "http"
    val defaultPort = 80
  } with slark.uri.UriSymbols {
    override def formatPath(path: List[String]) = path
  }
  
  val symbols = new {
    type P = byteParsers.type
    val parsers: byteParsers.type = byteParsers
    type UriSymbols = httpUriSymbols.type
    val uriSymbols: httpUriSymbols.type = httpUriSymbols
    val options = new Options {
      override def rejectBWSAfterStartLine = true
      override def rejectBWSAfterHeaderFieldName = true
    }
  } with HttpSymbols with DateTime

}