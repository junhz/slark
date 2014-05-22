package object slark {
  import scala.language.implicitConversions
  
  type tailrec = scala.annotation.tailrec
  
  type ^[+A, +B] = (A, B)
  val ^ = Tuple2
  
}