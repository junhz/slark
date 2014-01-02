import slark.logger._
import slark.diary._

package object configs {
  val log: Log.Factory = {
    case _: Log => Logger((Level.info, Formatter.default, Writer.console))
  }
  
  val diary: Diary.Paths = {
    case d: Diary => {
      import scala.reflect.runtime.universe
      val runtime = universe.runtimeMirror(getClass.getClassLoader)
      val rd = runtime.reflect(d)
      Resource.FileSystem("log/" + rd.symbol.fullName)
    }
  }
}