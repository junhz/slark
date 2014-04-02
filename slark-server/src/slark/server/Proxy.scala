package slark.server

import java.net.Socket
import scala.util.Random
import scala.collection.mutable.ListBuffer
import java.nio.ByteBuffer
import java.net.ServerSocket
import java.io.RandomAccessFile
import java.util.concurrent.Executors
import java.util.concurrent.Callable
import java.util.concurrent.TimeUnit
import scala.collection.immutable.TreeSet
import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import java.io.InputStream
import java.io.IOException
import java.io.File
import java.util.concurrent.atomic.AtomicInteger
import java.nio.channels.FileChannel
import java.net.SocketException
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import java.nio.channels.SocketChannel
import java.nio.channels.Selector
import java.nio.channels.SelectionKey
import java.nio.channels.Pipe
import sun.nio.ch.FileChannelImpl
//import scala.util.parsing.combinator.Parsers

object Proxy {
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) {

    val executor = Executors.newFixedThreadPool(4)
    val local = ServerSocketChannel.open()
    local.socket().bind(new InetSocketAddress(10086))

    while (true) {
      val client = local.accept
      executor.execute(new Runnable {
        val sockets = new Sockets(client)

        def affair: Boolean = {
          (try {
            Some(Request(read(in(client))))
          } catch {
            case _: Throwable => None
          }) match {
            case Some(request) => {
              request.headers.collectFirst {
                case header: Host => {
                  val server = sockets(header)
                  request.write(server)

                  readResponse(server)
                }
              } getOrElse false
            }
            case _ => {
              println(acc + "_" + sessionAcc + "request")
              false
            }
          }
        }

        def readResponse(server: SocketChannel): Boolean = {
          (try {
            Some(Response(read(in(server))))
          } catch {
            case _: Throwable => None
          }) match {
            case Some(response) => {
              response.write(client)
              true
            }
            case _ => {
              println(acc + "_" + sessionAcc + "response")
              false
            }
          }
        }

        override def run {
          while (affair) {
            sessionAcc += 1
          }
          sockets.closeAll
        }
      })
    }
  }
}