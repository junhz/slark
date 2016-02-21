package slark

import scala.collection.immutable.LinearSeq
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.LinearSeqOptimized
import scala.collection.AbstractSeq
import scala.collection.generic.SeqFactory
import scala.collection.generic.GenericCompanion
import scala.collection.mutable.Builder

sealed abstract class Stack[+A] extends AbstractSeq[A]
                                with LinearSeq[A]
                                with Product
                                with GenericTraversableTemplate[A, Stack]
                                with LinearSeqOptimized[A, Stack[A]] {
  override def companion: GenericCompanion[Stack] = Stack
}

final class StackBuilder[A] extends Builder[A, Stack[A]] {
  override def +=(elem: A): this.type = ???
  override def clear(): Unit = ???
  override def result(): Stack[A] = ???
}

object Stack extends SeqFactory[Stack] {
  override def newBuilder[A]: Builder[A, Stack[A]] = new StackBuilder
}