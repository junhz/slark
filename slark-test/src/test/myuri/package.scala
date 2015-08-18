package test

import slark.combinator.parser._
import slark.uri._

package object myuri {

  val charParsers = new Parsers with CharReaders
  
  val symbols = new { 
    val parsers: charParsers.type = charParsers
    val schemeName = "my"
    val defaultPort = 10086
  } with UriSymbols {
    type P = charParsers.type
    override def formatPath(path: List[String]): List[String] = path
  }
  
}