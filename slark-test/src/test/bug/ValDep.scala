package test.bug

object ValDep {

  trait A {
    type E
    val e: E
  }
  
  trait B {
    val a: A
    val b: a.type = a
    val e: a.E = b.e
  }
  
}