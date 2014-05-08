package slark
package http

import combinator.parser._

trait Message { self: Symbols[_ <: Parsers with OctetReaders with ImportChars[_ <: Parsers with uri.CharReaders]] with Literals =>

  protected[this] def _options: Options
  
  final val options: Options = _options
  
  import parsers._
  
  case class ResponseCode(code: Int, reason: String)
  
  protected[this] def _uriSymbols: Symbols[charParsers.type] with uri.Literals with uri.IPaddress with uri.Path with uri.Scheme
  
  final val uriSymbols: Symbols[charParsers.type] with uri.Literals with uri.IPaddress with uri.Path with uri.Scheme = _uriSymbols

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
    val bws = ows -> { ws => if(options.rejectBWSAfterStartLine || ws.isEmpty) fail() else succ() }
    ((request_line ^ bws :^ (header_field ^: crlf).*) ^: crlf) -> {
      case method ^ tar ^ ver ^ headers => HttpRequestDef(method, tar, ver, headers)
    }
  }

  val response = {
    val bws = ows -> { ws => if(options.rejectBWSAfterStartLine || ws.isEmpty) fail() else succ() }
    ((status_line ^ bws :^ (header_field ^: crlf).*) ^: crlf) -> {
      case ver ^ code ^ reason ^ headers => HttpResponseDef(ver, code, reason, headers)
    }
  }
  
  trait HttpHeaderDef[T, R] {
    def name: String
    def value: Parser[T]
    def collect(headers: List[(String, List[Byte])]): Option[R] = {
      val values = headers.collect {
        case (n, v) if (n.equalsIgnoreCase(name)) => value.parse(v)
      }
      val malformed = values.exists {
        case Succ(r, n) if (n.atEnd) => false
        case _ => true
      }
      if(malformed) None
      else flatten(values.map(_.asInstanceOf[Succ[T]].result))
    }
    def flatten(headers: List[T]): Option[R]
  }
  
  case class SingleHttpHeaderDef[T](name: String, value: Parser[T]) extends HttpHeaderDef[T, T] {
    override def flatten(headers: List[T]) = if(headers.isEmpty || !headers.tail.isEmpty) None else Some(headers.head)
  }
  
  case class MultipleHttpHeaderDef[T](name: String, value: Parser[List[T]]) extends HttpHeaderDef[List[T], List[T]] {
    override def flatten(headers: List[List[T]]) = {
      val h = headers.flatten
      if(h.isEmpty) None else Some(h)
    }
  }
  
  val content_length = SingleHttpHeaderDef("Content-Length", digit(1, `>`) -> { _ match { case Natural0(i) => i }})
  
  val transfer_encoding = MultipleHttpHeaderDef("Transfer-Encoding", token.`#`)
  
  implicit class ListOf[T](p: Parser[T]) {
    def `#` : Parser[List[T]] = {
      val leading = ows :^ p
      val tail = (ows ^ "," ^ ows) :^ p
      leading >> { x => (tail.*) -> { xs => x :: xs } }
    }
  }
  
}