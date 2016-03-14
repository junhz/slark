package slark.calculator._1

import scala.language.higherKinds

/**
 * @author a554114
 */
trait Notations {
  
}

trait BnfNotations {
  type Parser[T]
  type Seq[S, T]
  type Or[S, T]
  
  def ` `[S, T](lhs: Parser[S], rhs: Parser[T]): Parser[Seq[S, T]]
  
  def `|`[S, T >: S](lhs: Parser[S], rhs: Parser[T]): Parser[Or[S, T]]
}

trait CombinatorBnfNotations extends BnfNotations {
  type Seq[S, T] = (S, T)
  type Or[S, T] = T
}

trait LlBnfNotations extends BnfNotations {
  type Seq[S, T] = (S, T)
  type Or[S, T] = List[T]
}