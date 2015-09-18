package slark.script

import java.io.File
import scala.reflect.runtime
import scala.tools.reflect.ToolBox
import java.util.concurrent.ConcurrentHashMap
import java.io.FileInputStream
import java.util.Arrays
import java.io.InputStream
import java.io.BufferedReader
import java.io.InputStreamReader


object Interpreter {
  
  val loadedScripts: ConcurrentHashMap[String, Script] = new ConcurrentHashMap
  
  val scriptHome = sys.env.getOrElse("SCRIPT_HOME", ".")
  
  val tb = runtime.universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
  
  def main(args: Array[String]): Unit = {
    val script = load(args(0))
    val params = args.tail
    def input: Stream[String] = {
      val line = io.StdIn.readLine()
      if (line eq null) {
        Stream.Empty
      } else {
        line #:: input
      }
    }
    script(params:_*)(input) foreach println
  }
  
  def load(name: String): Script = {
    
    var script = loadedScripts.get(name)
    if (script == null) {
      
      val scriptLoc = new File(scriptHome, name)
      if (scriptLoc.exists()) {
        script = tb.eval(tb.parse(wrapCode(new String(readAll(new FileInputStream(scriptLoc)))))).asInstanceOf[Script]
        loadedScripts.putIfAbsent(name, script)
      } else {
        val in = classOf[Script].getClassLoader.getResourceAsStream(name)
        if (in ne null) {
          script = tb.eval(tb.parse(wrapCode(new String(readAll(in))))).asInstanceOf[Script]
          loadedScripts.putIfAbsent(name, script)
        } else {
          throw new IllegalArgumentException(s"script not found: $name")
        }
      }
      
    }
    if (System.console() ne null) {
      System.console().writer().println(s"$name loaded")
    }
    loadedScripts.get(name)
  }
  
  def wrapCode(code: String): String = {
    s"""new slark.script.Script {
       |  import slark.script.Interpreter.load
       |  import slark.script.Script._
       |  def apply(args: String*): (=>Stream[String]) => Stream[String] = {
       |    $code
       |  }
       |}""".stripMargin
  }
  
  def readAll(in: InputStream): Array[Byte] = {
    
    val buffer = new Array[Byte](1024)
    var size = in.read(buffer)
    var len = 0
    var saved = List[Array[Byte]]()
    while (size > 0) {
      
      len += size
      saved = Arrays.copyOf(buffer, size) :: saved
      size = in.read(buffer)
    }
    in.close()
    
    val bytes = new Array[Byte](len)
    while (!saved.isEmpty) {
      val head = saved.head
      saved = saved.tail
      size = head.length
      len -= size
      System.arraycopy(head, 0, bytes, len, size)
    }
    
    bytes
  }
  
}