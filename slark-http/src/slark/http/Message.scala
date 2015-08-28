package slark
package http

import combinator.parser._

trait Message { self: Literals =>
  
  val options: Options
  
  type UriSymbols <: uri.Literals with uri.IPaddress with uri.Path with uri.Scheme
  val uriSymbols: UriSymbols
  
  val encoder = new Encoder {
    type CharParsers = uriSymbols.parsers.type
    type ByteParsers = parsers.type
    
    val charParsers: uriSymbols.parsers.type = uriSymbols.parsers
    val byteParsers: parsers.type = parsers
  }
  
  import parsers._
  import encoder._
  
  case class ResponseCode(code: Int, reason: String)

  val http_version = ("HTTP/" :^ digit ^ "." :^ digit) -> { case major ^ minor => HttpVersion(major - '0', minor - '0') } | send(505, "HTTP Version Not Supported")
  
  final class HttpVersion private[HttpVersion](val major: Int, val minor: Int) extends Ordered[HttpVersion] {
    override def compare(that: HttpVersion) = (this.major - that.major) * 10 + (this.minor - that.minor)
    override def toString = s"HTTP/$major.$minor"
  }
  
  object HttpVersion {
    val `1.1` = new HttpVersion(1, 1)
    val `1.0` = new HttpVersion(1, 0)
    
    def apply(major: Int, minor: Int): HttpVersion = {
      require(major >= 0 && major < 10 && minor >= 0 && minor < 10)
      new HttpVersion(major, minor)
    }
  }
  
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
      case path ^ query => Origin(path, query.getOrElse(""))
    }
    val absolute_form = absolute_uri -> {
      case _ ^ path ^ query => Absolute(path, query.getOrElse(""))
    }
    val authority_form = authority -> (Authorize(_))

    val asterisk_form = "*" -> (_ => Asterisk)

    origin_form | absolute_form | authority_form | asterisk_form
  }

  val request_line = (method ^ sp :^ request_target ^ sp :^ http_version ^: crlf) | send(404, "Bad Request")

  val status_code = digit{3} -> { case Natural0(i) => i }

  val reason_phrase = (ht | sp | vchar | obs_text).* -> options.standarizeReasonPhrase

  val status_line = http_version ^ sp :^ status_code ^ sp :^ reason_phrase ^: crlf
  
  val header_field = token ^ (':' :^ ows) :^ (ht | sp | %(0x21, 0x7E)).* ^: ows

  trait HttpMessageDef
  case class HttpRequestDef(method: String, tar: RequestTarget, ver: HttpVersion, headers: List[(String, List[Byte])]) extends HttpMessageDef
  case class HttpResponseDef(ver: HttpVersion, code: Int, reason: String, headers: List[(String, List[Byte])]) extends HttpMessageDef
  
  val request = {
    val bws = ows -> { ws => if(options.rejectBWSAfterStartLine || ws.isEmpty) fail(BadWhiteSpace) else succ(()) }
    ((request_line ^ bws :^ (header_field ^: crlf).*) ^: crlf) -> {
      case method ^ tar ^ ver ^ headers => HttpRequestDef(method, tar, ver, headers)
    }
  }

  val response = {
    val bws = ows -> { ws => if(options.rejectBWSAfterStartLine || ws.isEmpty) fail(BadWhiteSpace) else succ(()) }
    ((status_line ^ bws :^ (header_field ^: crlf).*) ^: crlf) -> {
      case ver ^ code ^ reason ^ headers => HttpResponseDef(ver, code, reason, headers)
    }
  }
  
  case object BadWhiteSpace extends FailReason
  
}