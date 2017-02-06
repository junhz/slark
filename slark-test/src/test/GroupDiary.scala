package test

/**
 * @author a554114
 */
object GroupDiary extends Diary {
  
  val content = 
    Source(slark.Group(a = 1, b = 2)) ::
    Source(slark.Group(a = 1, b = "1")) :: Nil
}