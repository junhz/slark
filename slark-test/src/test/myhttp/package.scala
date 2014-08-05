package test

import slark.combinator.parser._
import slark.http._

package object myhttp {

  val acsiiParsers = new Parsers with slark.uri.CharReaders
  val byteParsers = new { val charParsers: acsiiParsers.type = acsiiParsers } with Parsers with OctetReaders with ImportChars[acsiiParsers.type]
  val httpUriSymbols = new { 
    val parsers: acsiiParsers.type = acsiiParsers
    val schemeName = "http"
    val defaultPort = 80
  } with slark.uri.UriSymbols[acsiiParsers.type] {
    override def formatPath(path: List[String]) = path
  }
  
  val symbols = new {
    val parsers: byteParsers.type = byteParsers
    val uriSymbols = httpUriSymbols
    val options = new Options {
      override def rejectBWSAfterStartLine = true
      override def rejectBWSAfterHeaderFieldName = true
    }
  } with HttpSymbols[acsiiParsers.type, byteParsers.type] with DateTime

}