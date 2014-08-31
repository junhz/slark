package slark.unique
package test

import slark.unique.Unique.unique

object Main {
  
  @unique class A
  type UniqueUnit = Unit @unique
  
  case class B[K, V](k: K, v: V)

  def f(a: String @unique): (String, String @unique) = {
    def g(a: String @unique)(b: String): (String @unique, String) = {
      (b, a)
    }
    val b = a
    a.charAt(0)
    val (b1, b2) = g(a)(a)
    (b2, b1)
  }
  
  def h = {
    val i: Int @unique = 1
    val b = B(i, B(1, 2))
    
    val B(b1, B(b2, b3)) = b
  }
  
  def refresh[@unique T](t: T): T = {
    t
  }
  
  def assignment[@unique T](t1: T, t2: T): T = {
    val x = 
      if (System.currentTimeMillis() % 2 == 1) {
        t1
      } else {
        t2
      }
    x
  }
  
  def tuple[T1, @unique T2](t: (T1, T2)): T2 = {
    val x = t._1
    val y = t._2
    y
  }
  
  def refer[T1, @unique T2](t1: T1, t2: T2, s: String @unique, a: A): UniqueUnit = {
    
  }
  
}