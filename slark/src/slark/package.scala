package object slark {
  import scala.language.implicitConversions
  
  type tailrec = scala.annotation.tailrec
  
  implicit def throwableToThrown(ex: Throwable): Thrown = new Thrown(ex)
  
}