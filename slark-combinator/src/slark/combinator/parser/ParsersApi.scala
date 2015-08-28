package slark
package combinator.parser

import scala.language.higherKinds

trait ParsersApi {
  type Input

  trait ParseResult[+S]
  
  trait FailApi extends ParseResult[Nothing]{
    def msg: List[FailReason]
  }
  
  type Fail <: FailApi
  
  trait FailExtractor {
    def apply(msg: List[FailReason]): Fail
    def unapply(f: Fail): Option[List[FailReason]]
  }
  
  val Fail: FailExtractor
  
  trait SuccApi[+S] extends ParseResult[S] {
    def result: S
    def next: Input
  }
  
  type Succ[+S] <: SuccApi[S]
  
  trait SuccExtractor {
    def apply[S](result: S, next: Input): Succ[S]
    def unapply[S](s: Succ[S]): Option[(S, Input)]
  }
  
  val Succ: SuccExtractor
  
  trait ParserApi[+S] {

    def parse(input: Input): ParseResult[S]

    /** flatmap */
    def >>[T](fn: S => Parser[T]): Parser[T]

    def |>[T >: S](fn: List[FailReason] => Parser[T]): Parser[T]

    /** plus */
    def |[T >: S](that: Parser[T]): Parser[T]

    /** map */
    def ->[T](fn: S => T): Parser[T]

    /** List */
    def ^[T](that: Parser[T]): Parser[S ^ T]

    /** guard */
    def :^[T](that: Parser[T]): Parser[T]

    /** guard */
    def ^:[T](that: Parser[T]): Parser[T]

    /** not */
    def ! : Parser[Unit]

    /** rep */
    def * : Parser[List[S]]

    /** rep */
    def apply(time: Int): Parser[List[S]]

    def apply(min: Int, max: Int): Parser[List[S]]

    /** option */
    def ? : Parser[Option[S]]
  }
  
  type Parser[+S] <: ParserApi[S]

  /** zero unit */
  def fail(msg: List[FailReason]): Parser[Nothing]

  /** unit */
  def succ[S](sym: S): Parser[S]

  /** max **/
  def `>`: Int
  /** min **/
  def `<`: Int

  val EOF: FailReason
  
  val MissingExpectedFailure: FailReason
}