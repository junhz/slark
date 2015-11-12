package slark.script

import java.io.File
import scala.reflect.runtime
import scala.tools.reflect.ToolBox
import java.util.concurrent.ConcurrentHashMap
import java.io.FileInputStream
import java.util.Arrays

object Compiler {
  
  def main(args: Array[String]): Unit = {
    args.foreach { arg => {
      try {
        Interpreter.load(arg)
        println(s"compile arg done.")
      } catch {
        case e: Throwable => println(s"compile arg failed."); e.printStackTrace() 
      }
    }}
  }
  
}