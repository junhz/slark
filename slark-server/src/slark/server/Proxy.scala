package slark.server

import java.util.concurrent.Executors
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import slark.uri._
import slark.combinator.parser.Parsers
import slark.http._
import java.nio.channels.SocketChannel
import java.nio.channels.Channel
import slark.combinator.future.Futures._
import java.nio.ByteBuffer

object Proxy {
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) {

    val executor = Executors.newFixedThreadPool(4)
    val local = ServerSocketChannel.open()
    local.socket().bind(new InetSocketAddress(10086))

    val charParsers = new Parsers with CharReaders

    val uriSymbols = new UriSymbols[Parsers with CharReaders] {
      protected[this] override def _parsers = charParsers
      protected[this] override def _name = "http"
      protected[this] override def _port = 80
      protected[this] override def formatPath(path: List[String]): List[String] = path
    }

    val byteParsers = new Parsers with OctetReaders with ImportChars[Parsers with slark.uri.CharReaders] {
      protected[this] override def _charParsers = charParsers
    }

    val httpSymbols = new HttpSymbols[Parsers with OctetReaders with ImportChars[Parsers with slark.uri.CharReaders]] { self =>
      protected[this] override def _parsers = byteParsers
      protected[this] override def _uriSymbols = uriSymbols

      protected[this] override def _options = new Options {
        override def rejectBWSAfterStartLine = true
        override def rejectBWSAfterHeaderFieldName = true
      }
    }

    type Addr = (uriSymbols.Host, Int)
    val Addr = Tuple2

    implicit val addrOrd = new Ordering[Addr] {
      override def compare(lhs: Addr, rhs: Addr) = {
        val Addr(host1, port1) = lhs
        val Addr(host2, port2) = rhs
        val hostDiff = host1.name compareTo host2.name
        if (hostDiff != 0) hostDiff else port1 - port2
      }
    }

    def openSocketChannel(k: Addr): SocketChannel = SocketChannel.open(new InetSocketAddress(k._1.name, k._2))

    def closeChannel(channel: Channel): Unit = channel.close()

    def isChannelOpen(channel: Channel): Boolean = channel.isOpen()

    implicit val socketToReader = (sc: SocketChannel) => {
      val buffer = ByteBuffer.allocate(1024)

      class SocketReader extends byteParsers.Reader {
        var hd: Byte = _
        var tl: SocketReader = _
        var filled = false

        def fill: Boolean = {
          if (buffer.hasRemaining()) {
            hd = buffer.get()
            tl = new SocketReader
            filled = true
            true
          } else {
            if (sc.isOpen()) {
              buffer.clear()
              val size = sc.read(buffer)
              fill
            } else {
              false
            }
          }
        }

        override def atEnd = filled | fill 
        override def head = if(atEnd) ??? else hd
        override def tail = if(atEnd) ??? else tl
      }
    }

    while (true) {
      val client = local.accept
      val sockets = new Pool(openSocketChannel, closeChannel, isChannelOpen)

      future {

      }

      executor.execute(new Runnable {
        /*def affair: Boolean = {
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
              println(acc+"_"+sessionAcc+"request")
              false
            }
          }
        }*/

        /*def readResponse(server: SocketChannel): Boolean = {
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
              println(acc+"_"+sessionAcc+"response")
              false
            }
          }
        }*/

        override def run {
          /*while (affair) {
            sessionAcc += 1
          }
          sockets.closeAll*/
        }
      })
    }
  }
}