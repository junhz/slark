package main

object Main {

  def main(args: Array[String]): Unit = {
    val s = slark.Source {
      def anno(x: String) = x + " "
      anno _
    }
    println(s.srcTree)
  }

  trait B {
    object B {
      def f = {
        
        val slim$B$this = slark.slim.Slim.outer[B]
        val slim$B$B$this = slark.slim.Slim.outer[B.type]
        
        slark.slim.Slim {
          //(x: String, y: Int) => x + slim$B$this.i + slim$B$this.j + slim$B$this.k + slim$B$B$this.ii + slim$B$B$this.jj + slim$B$B$this.kk + slim$B$this.H + slim$B$B$this.HH + x.charAt(y)
          (x: String, y: Int) => x + i + j + k + ii + jj + kk + H + HH + x.charAt(y)
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