import slark.logger._

package object configs {
  val log: Log.Factory = {
    case slark.parser.`package` => Logger((Level.warn, Formatter.default, Writer.console))
    case _ => Logger.silent
  }
}