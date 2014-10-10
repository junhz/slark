package slark.script

import java.io.File
import scala.reflect.runtime
import scala.tools.reflect.ToolBox
import java.util.concurrent.ConcurrentHashMap
import java.io.FileInputStream
import java.util.Arrays

object Interpreter {
  
  val loadedScripts: ConcurrentHashMap[String, Script] = new ConcurrentHashMap
  
  val scriptHome = sys.env.getOrElse("SCRIPT_HOME", ".")
  
  val tb = runtime.universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
  
  def main(args: Array[String]): Unit = {
    println(load(args(0))(args.tail:_*))
  }
  
  def load(name: String): Script = {
    
    var script = loadedScripts.get(name)
    if (script == null) {
      
      val scriptLoc = new File(scriptHome, name)
      script = tb.eval(tb.parse(wrapCode(new String(readAll(scriptLoc))))).asInstanceOf[Script]
      loadedScripts.putIfAbsent(name, script)
    }
    
    loadedScripts.get(name)
  }
  
  def wrapCode(code: String): String = {
    s"""new slark.script.Script {
       |  import slark.script.Interpreter.load
       |  def apply(args: String*) = {
       |    $code
       |  }
       |}""".stripMargin
  }
  
  def readAll(f: File): Array[Byte] = {
    
    val in = new FileInputStream(f);
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