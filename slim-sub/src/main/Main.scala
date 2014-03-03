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
            () => i
          }
        }

        val i = ""
      }
    }

  }

}