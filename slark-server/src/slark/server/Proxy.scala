package slark
package server

import java.util.concurrent.Executors
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import uri._
import combinator.parser.Parsers
import combinator.collector.Collectors
import http._
import java.nio.channels.SocketChannel
import java.nio.channels.Channel
import slark.combinator.runnable.Runnables._
import java.nio.ByteBuffer
import java.nio.channels.Channels

object Proxy {
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) {

    val executor = Executors.newFixedThreadPool(4)
    val local = ServerSocketChannel.open()
    local.socket().bind(new InetSocketAddress(10086))

    val acsiiParsers = new Parsers with CharReaders

    val httpUriSymbols = new UriSymbols[acsiiParsers.type] {
      protected[this] override def _parsers = acsiiParsers
      protected[this] override def _name = "http"
      protected[this] override def _port = 80
      protected[this] override def formatPath(path: List[String]): List[String] = path
    }

    val byteParsers = new Parsers with OctetReaders with ImportChars[acsiiParsers.type] {
      protected[this] override def _charParsers = acsiiParsers
    }

    val httpSymbols = new HttpSymbols[acsiiParsers.type, byteParsers.type] { self =>
      protected[this] override def _parsers = byteParsers
      protected[this] override def _uriSymbols = httpUriSymbols

      protected[this] override def _options = new Options {
        override def rejectBWSAfterStartLine = true
        override def rejectBWSAfterHeaderFieldName = true
      }
    }
    val hs = httpSymbols
    
    val headerCollectors = new Collectors with HeaderReaders {
      type CharParsers = acsiiParsers.type
      type ByteParsers = byteParsers.type
      type HttpSymbols = hs.type
  
      val httpSymbols: HttpSymbols = hs
    }
    import headerCollectors._
    import byteParsers._
    import httpSymbols._
    import httpUriSymbols.Host
    
    val content_length = "Content-Length" `: ` (digit(1, `>`) -> { _ match { case Natural0(i) => i }})
  
    val transfer_encoding = "Transfer-Encoding" `: #` token


    type Addr = (Host, Int)
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

      val it = new Iterator[Byte] {
        val buffer = ByteBuffer.allocate(1024)
        buffer.flip()
        var cnt: Byte = _
        var filled = false
        var reachEOF = false

        @tailrec
        private[this] final def fill: Boolean = {
          if (buffer.hasRemaining()) {
            cnt = buffer.get()
            filled = true
            true
          } else {
            buffer.clear()
            val size = sc.read(buffer)
            if (size == -1) {
              reachEOF = true
              false
            } else {
              buffer.flip()
              fill
            }
          }
        }

        override def hasNext = filled || (!reachEOF && sc.isOpen() && fill)

        override def next = if (hasNext) {
          filled = false
          cnt
        } else ???

      }

      class SocketReader extends byteParsers.Reader {
        var hd: Byte = _
        var tl: SocketReader = _
        var filled = false

        def fill: Boolean = {
          if (it.hasNext) {
            hd = it.next
            tl = new SocketReader
            filled = true
            true
          } else false
        }

        override def atEnd = !(filled || fill)
        override def head = {
          if (atEnd) ??? else hd
        }
        override def tail = if (atEnd) ??? else tl
      }

      (new SocketReader).asInstanceOf[byteParsers.Reader]
    }
    
    trait HttpMessageBody
    
    object Chunked extends HttpMessageBody
    case class Fixed(len: Int) extends HttpMessageBody
    object ToEOF extends HttpMessageBody

    def error(code: Int, reason: String): HttpResponseDef = ???

    def sendMsg(msg: HttpMessageDef, sc: SocketChannel): Unit = ???

    while (true) {
      val client = local.accept
      val sockets = new Pool(openSocketChannel, closeChannel, isChannelOpen)

      import byteParsers._
      
      val proxy = runnable { (request: HttpRequestDef) => {
        println(request)
        (transfer_encoding map { _ => Chunked }) | (content_length map { len => Fixed(len) }) | collected(Fixed(0)) collect request.headers match {
          case Collected(r, n) => { println(r); println(n) }
          case Malformed(msg) => println(msg)
        }
      }}
      
      request -> { req => deploy(proxy, req, executor); () } |> { msg => succ(println(msg)) } parse client
    }
  }
}