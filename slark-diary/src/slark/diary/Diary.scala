package slark
package diary

import scala.reflect.macros.Context
import scala.language.experimental.macros

trait Diary {

  def content: List[Source[Any]]

  final def main(args: Array[String]) {
    val resource = Diary.paths.lift.apply(this).getOrElse(Resource.Empty)
    resource.write(resource.read(source => {
      val modified = content.map(code => {
        (code.srcTree, code.output match {
          case Left(t) => t match {
            case null => "null"
            case _ => t.toString
          }
          case Right(ex) => ex.mkString("\r\nCaused By: ")
        })
      })
      Merger.SimpleAutoMerger(Diff.MyersDiff(source, modified)(_._1 equals _._1), source, modified)
    }))
  }

}

object Diary {
  type Paths = PartialFunction[Diary, Resource]

  lazy val paths: Paths = {
    try {
      import scala.reflect.runtime.universe
      val runtime = universe.runtimeMirror(getClass.getClassLoader)
      val configs = runtime.staticModule("configs.package")
      val diary = configs.typeSignature.declaration(universe.newTermName("diary")).asTerm
      runtime.reflect(runtime.reflectModule(configs).instance).reflectField(diary).get.asInstanceOf[Paths]
    } catch {
      case e: Throwable => {
        e.printStackTrace()
        ({ case d: Diary => Resource.Empty })
      }
    }
  }
}