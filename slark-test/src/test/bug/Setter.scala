package test.bug

object Setter {

  trait A {
    val i: Int = 1
    val j: Int
  }
  
  new A{ val j = 1 }
  
}