package test
package mylogger

import slark.logger.Level._

object LevelDiary extends Diary {

  val content = Source(info reaches info) ::
    Source(info reaches warn) ::
    Source(info reaches error) ::
    Source(warn reaches info) ::
    Source(warn reaches warn) ::
    Source(warn reaches error) ::
    Source(error reaches info) ::
    Source(error reaches warn) ::
    Source(error reaches error) :: Nil

}