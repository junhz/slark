package slark
package http

import combinator.parser._

trait Message { self: Symbols[Parsers with OctetReaders with ImportChars[Parsers with uri.CharReaders]] with Literals =>

  import parsers._
  
  case class ResponseCode(code: Int, reason: String)
  
  protected[this] def _uriSymbols: Symbols[charParsers.type] with uri.Literals with uri.IPaddress with uri.Path with uri.Scheme
  
  final val uriSymbols: Symbols[charParsers.type] with uri.Literals with uri.IPaddress with uri.Path with uri.Scheme = _uriSymbols

  val http_version = ("HTTP/" :^ digit{1} ^ "." :^ digit{1}) | send(505, "HTTP Version Not Supported")

  val uri_reference: Parser[uriSymbols.UriReference] = uriSymbols.uri_reference
  
  val absolute_uri: Parser[((String, uriSymbols.Part), Option[String])] = uriSymbols.absolute_uri
  
  val relative_part: Parser[uriSymbols.Part] = uriSymbols.relative_part
  
  val scheme: Parser[String] = uriSymbols.scheme
  
  val authority: Parser[uriSymbols.Authority] = uriSymbols.authority
  
  val uri_host: Parser[uriSymbols.Host] = uriSymbols.host
  
  val port: Parser[Int] = uriSymbols.port
  
  val path_abempty: Parser[List[String]] = uriSymbols.path_abempty
  
  val segment: Parser[String] = uriSymbols.segment
  
  val query: Parser[String] = uriSymbols.query
  
  val fragment: Parser[String] = uriSymbols.fragment
  
  val absolute_path = ("/" :^ segment)(1, `>`)
  
  val partial_uri = relative_part ^ ("?" :^ query).?

  val method = token

  trait RequestTarget
  case class Origin(path: List[String], query: String) extends RequestTarget
  case class Absolute(part: uriSymbols.Part, query: String) extends RequestTarget
  case class Authorize(auth: uriSymbols.Authority) extends RequestTarget
  case object Asterisk extends RequestTarget

  val request_target = {

    val origin_form = (absolute_path ^ ("?" :^ query).?) -> {
      case (path, query) => Origin(path, query.getOrElse(""))
    }
    val absolute_form = absolute_uri -> {
      case ((_, path), query) => Absolute(path, query.getOrElse(""))
    }
    val authority_form = authority -> (Authorize(_))

    val asterisk_form = "*" -> (_ => Asterisk)

    origin_form | absolute_form | authority_form | asterisk_form
  }

  val request_line = method ^ sp :^ request_target ^ sp :^ http_version ^: crlf

  val status_code = digit{3}

  val reason_phrase = (ht | sp | vchar).*

  val status_line = http_version ^ sp :^ status_code ^ sp :^ reason_phrase ^: crlf
  
  val header_field = token ^ (':' :^ ows) :^ (ht | sp | %(0x21, 0x7E)).* ^: ows

  val request = (request_line ^ (header_field ^: crlf).*) ^: crlf

  val response = (status_line ^ (header_field ^: crlf).*) ^: crlf
}