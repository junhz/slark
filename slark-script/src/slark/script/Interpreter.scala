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
import scala.annotation.tailrec
import java.text.SimpleDateFormat


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
    var seq = script(params:_*)(input)
    while (!seq.isEmpty) {
      println(seq.head)
      seq = seq.tail
    }
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
      if (System.console() ne null) {
        System.console().writer().println(s"$name loaded")
      }
    }
    
    loadedScripts.get(name)
  }
  
  def wrapCode(code: String): String = {
    s"""new slark.script.Script {
       |  import slark.script.Interpreter.load
       |  import slark.script.Script._
       |  def apply(args: String*): (=>Seq[String]) => Seq[String] = {
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

object Test extends Script {
  import slark.script.Interpreter.load
  import slark.script.Script._
  override def apply(args: String*) = {
val `cat` = load("cat")()
val df = new java.text.SimpleDateFormat("dd MMM yyyy HH:mm:ss,SSS")
val start = `cat`(s"${args(0)}/${args(1)}_start" :: Nil).grouped(2).map(s => (s(1), df.parse(s(0)).getTime)).toList
val end = `cat`(s"${args(0)}/${args(1)}_end" :: Nil).grouped(2).map(s => (s(1), df.parse(s(0)).getTime)).toList
def merge(start: List[(String, Long)], 
          end: List[(String, Long)],
          merged: List[(String, Long)]): List[String] = {
  if (end.isEmpty) merged.reverse.flatten(t => t._1 :: t._2.toString() :: Nil)
  else {
    val head = end.head
    find(head._2, start, Nil) match {
      case Some(s) => merge(s.tail, end.tail, (head._1, head._2 - s.head._2) :: merged)
      case _ => throw new IllegalArgumentException(s"can't find start of $head")
    }
  }
}
def find(end: Long, start: List[(String, Long)], pre: List[(String, Long)]): Option[List[(String, Long)]] = {
  if (start.isEmpty) pre match {
      case t :: ts => Some(t :: start)
      case _ => println("end of start"); None
  } else {
    val head = start.head
    if (head._2 > end) pre match {
      case t :: ts => Some(t :: start)
      case _ => println("end of end"); None
    } else find(end, start.tail, head :: pre)
  }
}
_ => merge(start, end, Nil)

  }
}