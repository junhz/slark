package main

object Main {

  def main(args: Array[String]): Unit = {
    val s = slark.Source {
      final class Slim extends scala.runtime.AbstractFunction1[String, String] {
        override def apply(x: String) = x
      }
      new Slim()
    }
    println(s.srcTree)
  }

  trait A {
    object B {
      def f = {
        val iii = ""
        def jjj = ""
        var kkk = ""
        object HHH

        slark.slim.Slim {
          (x: String, y: Int) => x + i + j + k + ii + jj + kk + iii + jjj + kkk + H + HH + HHH + x.charAt(y)
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