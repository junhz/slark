package slark
package combinator.collector

trait Collectors extends AbstractInput {
  
  trait CollectResult[+T]
  case class Collected[T](t: T, rest: Input) extends CollectResult[T]
  case object NotFound extends CollectResult[Nothing]
  case class Malformed(msg: String) extends CollectResult[Nothing]
  
  trait Collector[+T] { self =>
    def collect(input: Input): CollectResult[T]
    final def | [U >: T](that: Collector[U]): Collector[U] = new Collector[U] {
      override def collect(input: Input) = self collect input match {
        case NotFound => that collect input
        case r => r
      }
    }
    final def map[U](f: T => U): Collector[U] = new Collector[U] {
      override def collect(input: Input) = self collect input match {
        case Collected(t, r) => Collected(f(t), r)
        case NotFound => NotFound
        case Malformed(msg) => Malformed(msg)
      }
    }
    final def flatMap[U](f: T => Collector[U]): Collector[U] = new Collector[U] {
      override def collect(input: Input) = self collect input match {
        case Collected(t, r) => f(t) collect r
        case NotFound => NotFound
        case Malformed(msg) => Malformed(msg)
      }
    }
  }
  
  final def collected[T](t: T): Collector[T] = new Collector[T] {
    override def collect(input: Input) = Collected(t, input)
  }
  final def notFound[T]: Collector[T] = new Collector[T] {
    override def collect(input: Input) = NotFound
  }
  final def malformed[T](msg: String): Collector[T] = new Collector[T] {
    override def collect(input: Input) = Malformed(msg)
  }
}