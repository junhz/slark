package junhz
package http

trait ContentCodings {

  trait ContentCoding
  object ContentCoding {
    def unapply(token: List[Byte]): Option[ContentCoding] = ???
  }

  case class ContentCodingRegistery(name: String)

}