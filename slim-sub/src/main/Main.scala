package main

object Main {

  def main(args: Array[String]): Unit = {

    val origin = slark.Source {
      trait A {
        trait B {
          def f = () => i
        }

        val i = 1
      }
    }

    val trim = slark.Source {
      trait A {
        trait B {
          def f = slark.slim.Slim {
            (_: String) => i + j + k + ii + jj + kk
          }
          
          val ii = ""
          def jj = ""
          var kk = ""
        }

        val i = ""
        def j = ""
        var k = ""
      }
    }

  }

}