package slark
package combinator.parser

import scala.language.higherKinds

trait ResultsApi {
  type Input
  
  trait Result[+S]
  
  trait FailApi extends Result[Nothing]{
    def msg: List[FailReason]
  }
  
  type Fail <: FailApi
  
  trait FailExtractor {
    def apply(msg: List[FailReason]): Fail
    def unapply(f: Fail): Option[List[FailReason]]
  }
  
  val Fail: FailExtractor
  
  trait SuccApi[+S] extends Result[S] {
    def result: S
    def next: Input
  }
  
  type Succ[+S] <: SuccApi[S]
  
  trait SuccExtractor {
    def apply[S](result: S, next: Input): Succ[S]
    def unapply[S](s: Succ[S]): Option[(S, Input)]
  }
  
  val Succ: SuccExtractor
}