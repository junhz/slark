import slark.logger._

package object configs {
  val log: Log.Factory = {
    case _ => Logger.silent
  }
}