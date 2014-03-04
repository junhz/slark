package main

object Main {

  def main(args: Array[String]): Unit = {
    val s = slark.Source {
      final class Slim(val i: String) extends scala.runtime.AbstractFunction0[String] {
        override def apply = i
      }
      new Slim("")
    }
    println(s.srcTree)
  }

  object A {
    object B {
      def f = {
        val iii = ""
        def jjj = ""
        var kkk = ""
        object HHH

        slark.slim.Slim {
          () => i + j + k + ii + jj + kk + iii + jjj + kkk + H + HH + HHH
        }
      }

      val ii = ""
      def jj = ""
      var kk = ""
      object HH
    }

    val i = ""
    def j = ""
    var k = ""
    object H
  }

}