package slark.combinator.parser

import scala.annotation.tailrec

class Tail {

  val head: Option[Tail] = None
  
  @tailrec
  private final def me: String = {
    head match {
      case None => ""
      case Some(t) => t.me
    }
  }
  
  @tailrec
  private final def mine(i: Int): String = {
    head match {
      case None => ""
      case Some(t) => t.mine(i)
    }
  }
  
}