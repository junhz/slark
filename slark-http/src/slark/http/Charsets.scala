package slark.http

trait Charsets {

  trait Charset
  object Charset {
    def unapply(token: List[Byte]): Option[Charset] = ???
  }

}