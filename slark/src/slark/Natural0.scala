package slark

object Natural0 {
  def unapply(str: String): Option[Int] = {
    @tailrec
    def rec(index: Int, result: Int): Option[Int] = {
      if (str.length() == index) Some(result)
      else {
        unapply(str.charAt(index)) match {
          case None => None
          case Some(i) => rec(index + 1, result * 10 + i)
        }
      }
    }
    rec(0, 0)
  }

  def unapply(src: List[Byte]): Option[Int] = {
    @tailrec
    def rec(rest: List[Byte], result: Int): Option[Int] = {
      if (rest.isEmpty) Some(result)
      else {
        unapply(rest.head.toChar) match {
          case None => None
          case Some(i) => rec(rest.tail, result * 10 + i)
        }
      }
    }
    rec(src, 0)
  }

  def unapply(c: Char): Option[Int] = {
    val cnt = c - '0'
    if (cnt < 0 || cnt > 9) None
    else Some(cnt)
  }

  object Hex {
    def unapply(str: String): Option[Int] = {
      @tailrec
      def rec(index: Int, result: Int): Option[Int] = {
        if (str.length() == index) Some(result)
        else {
          unapply(str.charAt(index)) match {
            case None => None
            case Some(i) => rec(index + 1, result << 4 | i)
          }
        }
      }
      rec(0, 0)
    }

    def unapply(src: List[Char]): Option[Int] = {
      @tailrec
      def rec(rest: List[Char], result: Int): Option[Int] = {
        if (rest.isEmpty) Some(result)
        else {
          unapply(rest.head) match {
            case None => None
            case Some(i) => rec(rest.tail, result << 4 | i)
          }
        }
      }
      rec(src, 0)
    }

    def unapply(c: Char): Option[Int] = {
      if (c >= '0' && c <= '9') Some(c - '0')
      else if (c >= 'A' && c <= 'F') Some(c - 'A' + 10)
      else if (c >= 'a' && c <= 'f') Some(c - 'a' + 10)
      else None
    }

  }

}