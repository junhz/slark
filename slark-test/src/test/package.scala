import slark.logger.Log

package object test extends Log {
  private[test] lazy val log = newLogger
  type Diary = slark.diary.Diary
  val Source = slark.Source
}