package slark
package http

import combinator.parser._

trait Message { self: Symbols[Parsers with OctetReaders with ImportChars[Parsers with uri.CharReaders]] with Literals =>

  import parsers._
  
  val uriSymbols: Symbols[charParsers.type] with uri.Literals with uri.IPaddress with uri.Path with uri.Scheme

  val http_version = "HTTP/" :^ digit(1, `>`) ^ "." :^ digit(1, `>`)

  lazy val http_uri: Parser[uriSymbols.UriReference] = uriSymbols.uri_reference

  val header_field = token ^ (':' :^ ows) :^ (ht | sp | %(0x21, 0x7E)).* ^: ows

  val method = token

  trait RequestTarget
  case class Origin(path: List[String], query: String) extends RequestTarget
  case class Absolute(part: uriSymbols.Part, query: String) extends RequestTarget
  case class Authorize(auth: uriSymbols.Authority) extends RequestTarget
  case object Asterisk extends RequestTarget

  lazy val request_target = {

    val origin_form: Parser[RequestTarget] = (p(uriSymbols.path_absolute) ^ ("?" :^ uriSymbols.query).?) -> {
      case (path, query) => Origin(path, query.getOrElse(""))
    }
    val absolute_form: Parser[RequestTarget] = p(uriSymbols.absolute_uri) -> {
      case ((_, path), query) => Absolute(path, query.getOrElse(""))
    }
    val authority_form: Parser[RequestTarget] = p(uriSymbols.authority) -> (Authorize(_))

    val asterisk_form: Parser[RequestTarget] = p("*") -> (_ => Asterisk)

    origin_form | absolute_form | authority_form | asterisk_form
  }

  lazy val request_line = method ^ sp :^ request_target ^ sp :^ http_version ^: crlf

  val status_code = digit{3}

  val reason_phrase = (ht | sp | %(0x21, 0x7E)).*

  val status_line = http_version ^ sp :^ status_code ^ sp :^ reason_phrase ^: crlf

  lazy val request = (request_line ^ (header_field ^: crlf).*) ^: crlf

  lazy val response = (status_line ^ (header_field ^: crlf).*) ^: crlf
}

object Message {
  abstract class AbstractMessage[+P <: Parsers with OctetReaders with ImportChars[Parsers with uri.CharReaders]](
    val parsers: P) extends Symbols[P] with Literals with Message
}