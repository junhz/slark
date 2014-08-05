package test

import slark.combinator.parser._
import slark.uri._

package object myuri {

  val symbols = new { 
    val parsers = new Parsers with CharReaders
    val schemeName = "my"
    val defaultPort = 10086
  } with UriSymbols[Parsers with CharReaders] {
    override def formatPath(path: List[String]): List[String] = path
  }
  
}