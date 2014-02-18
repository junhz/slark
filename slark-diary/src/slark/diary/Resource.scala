package slark
package diary

import java.io.File
import java.nio.ByteBuffer
import java.io.FileInputStream
import java.io.FileOutputStream

trait Resource {

  type Underlying

  val underlying: Underlying

  def read[U](f: List[(String, String)] => U): U

  def write(output: => List[(String, String)]): Unit

}

object Resource {

  case class FileSystem(dir: String) extends Resource {

    type Underlying = File

    override val underlying = {
      val root = new File(dir)
      if (!root.exists()) require(root.mkdirs())
      else require(root.isDirectory())
      root
    }

    override def read[U](f: List[(String, String)] => U): U = {
      @tailrec
      def r(count: Int, stack: List[(String, String)]): List[(String, String)] = {
        val next = new File(underlying, Integer.toString(count))
        if (next.exists() && next.isDirectory()) {
          val source = new File(next, "source")
          if (source.exists() && source.isFile()) {
            val output = new File(next, "output")
            if (output.exists() && output.isFile()) r(count + 1, (readAll(source), readAll(output)) :: stack)
            else r(count + 1, (readAll(source), "") :: stack)
          } else stack.reverse
        } else stack.reverse
      }

      f(r(0, Nil))
    }

    override def write(merged: => List[(String, String)]): Unit = {
      @tailrec
      def rec(merged: List[(String, String)], count: Int): Unit = {
        if (merged.isEmpty) {}
        else {
          val next = new File(underlying, Integer.toString(count))
          next.mkdirs()
          val source = new FileOutputStream(new File(next, "source")).getChannel()
          source.lock()
          source.write(ByteBuffer.wrap(merged.head._1.getBytes()))
          source.close()
          val output = new FileOutputStream(new File(next, "output")).getChannel()
          output.lock()
          output.write(ByteBuffer.wrap(merged.head._2.getBytes()))
          output.close()

          rec(merged.tail, count + 1)
        }
      }
      val m = merged
      underlying.renameTo(new File(underlying.getParent(), System.nanoTime()+".back"))
      rec(m, 0)
    }

    def readAll(f: File): String = {
      val in = new FileInputStream(f).getChannel()
      val size = in.size()
      val buffer = ByteBuffer.allocate(size.toInt)
      in.read(buffer)
      in.close()
      new String(buffer.array(), "utf-8")
    }
  }

  object Empty extends Resource {
    type Underlying = Any
    val underlying = null
    def read[U](f: List[(String, String)] => U): U = f(Nil)

    def write(output: => List[(String, String)]): Unit = {
      @tailrec
      def rec(output: List[(String, String)]) {
        if (output.isEmpty) {}
        else {
          println(s"${output.head._1} -> ${output.head._2}")
          rec(output.tail)
        }
      }
      rec(output)
    }
  }

}