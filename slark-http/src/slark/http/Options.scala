package slark.http

trait Options {

  // obs-text
  def standarizeReasonPhrase(reasonPhrase: List[Byte]): String = reasonPhrase.foldLeft(new StringBuilder)((sb, b) => { sb.append(b.toChar); sb }).toString

  /**
   * A recipient that receives whitespace between the
   * start-line and the first header field MUST either reject the message
   * as invalid or consume each whitespace-preceded line without further
   * processing of it (i.e., ignore the entire line, along with any
   * subsequent lines preceded by whitespace, until a properly formed
   * header field is received or the header section is terminated).
   * @see <a href="http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-26#section-3">Section 3 of Part1</a>
   */
  def rejectBWSAfterStartLine: Boolean

  /**
   * Recipients of an invalid request-line SHOULD respond with either a
   * 400 (Bad Request) error or a 301 (Moved Permanently) redirect with
   * the request-target properly encoded.
   * @see <a href="http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-26#section-3.1.1">Section 3.1.1 of Part1</a>
   */
  final def rejectBWSInRequestTarget: Boolean = true

  /**
   * A server that receives a method
   * longer than any that it implements SHOULD respond with a 501 (Not
   * Implemented) status code.  A server that receives a request-target
   * longer than any URI it wishes to parse MUST respond with a 414 (URI
   * Too Long) status code
   * @see <a href="http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-26#section-3.1.1">Section 3.1.1 of Part1</a>
   */
  final def requestLineLengthLimit = Int.MaxValue

  /**
   * A server MUST reject any received request message that contains
   * whitespace between a header field-name and colon with a response code
   * of 400 (Bad Request).  A proxy MUST remove any such whitespace from a
   * response message before forwarding the message downstream.
   * @see <a href="http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-26#section-3.2.4">Section 3.2.4 of Part1</a>
   */
  def rejectBWSAfterHeaderFieldName: Boolean
}