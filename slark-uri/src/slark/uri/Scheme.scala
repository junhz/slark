package slark
package uri

import combinator.parser._
import FuncLib._
import java.net.InetSocketAddress

trait Scheme { self: Symbols[Parsers with CharReaders] with Literals with IPaddress with Path =>

  val schemeName: String

  val defaultPort: Int

  def formatPath(path: List[String]): List[String]

  import parsers._

  val scheme: Parser[String] = {
    (alpha ^ (alpha | digit | '+' | '-' | '.').*) parse schemeName match {
      case Succ(_, n) if (n.atEnd) => schemeName.toLowerCase.ignoreCase
      case _ => throw new IllegalArgumentException(s"invalid scheme name ${schemeName}")
    }
  }

  val reg_name = (unreserved | pct_encoded | sub_delims).* -> (_.mkString.toLowerCase())

  trait Host {
    // quick and dirty hack
    def name: String
  }

  object Host {
    def apply(ipv4: IPv4address): Host = new Host {
      override def toString = ipv4.toString
      override def name = ipv4.toString
    }

    def apply(ipv6: IPv6address): Host = new Host {
      override def toString = s"[$ipv6]"
      override def name = ipv6.toString
    }

    def apply(ipvfuture: IPvFuture): Host = new Host {
      override def toString = s"[$ipvfuture]"
      override def name = ipvfuture.toString
    }

    def apply(regname: String): Host = new Host {
      override def toString = regname
      override def name = regname
    }
    val localhost = apply("localhost")
  }

  val host = ("[" :^ (ipv6address -> (Host(_)) | ipvFuture -> (Host(_))) ^: "]") | ipv4address -> (Host(_)) | reg_name -> (Host(_))

  val fragment = (pchar | '/' | '?').* -> (_.mkString)

  val query = (pchar | '|' | '?').* -> (_.mkString)

  val userinfo = (unreserved | pct_encoded | sub_delims | ':').* -> (_.mkString)

  val port = digit(1, `>`) -> { case Natural0(i) => i } | succ(defaultPort)

  trait Authority {

  }

  object Authority {
    def annoymous(host: Host, port: Int): Authority = new Authority {
      override def toString = s"$host:$port"
    }
    def as(userinfo: String, host: Host, port: Int) = new Authority {
      override def toString = s"$userinfo@$host:$port"
    }
  }

  val authority = {
    ((userinfo ^: "@").? ^ host ^ (":" :^ port).?) -> {
      case None ^ host ^ None => Authority.annoymous(host, defaultPort)
      case None ^ host ^ Some(port) => Authority.annoymous(host, port)
      case Some(userinfo) ^ host ^ None => Authority.as(userinfo, host, defaultPort)
      case Some(userinfo) ^ host ^ Some(port) => Authority.as(userinfo, host, port)
    }
  }

  trait UriReference
  object UriReference {
    def apply(schema: String, part: Part, query: String, fragment: String): UriReference = new UriReference {
      override def toString = s"$schema:$part?$query#$fragment"
    }
    def apply(part: Part, query: String, fragment: String): UriReference = new UriReference {
      override def toString = s"$part?$query#$fragment"
    }
  }

  trait Part
  object Part {
    def network(authority: Authority, path: List[String]): Part = new Part {
      override def toString = s"//$authority${path.map("/" + _).mkString}"
    }
    def absolute(path: List[String]): Part = new Part {
      override def toString = path.map("/" + _).mkString
    }
    def relative(path: List[String]): Part = new Part {
      override def toString = path.mkString("/")
    }
    val empty: Part = new Part { override def toString = "" }
  }

  val relative_part = (
    ("//" :^ authority ^ path_abempty) -> { case authority ^ path => Part.network(authority, path) } |
    path_absolute -> (Part.absolute(_)) |
    path_noscheme -> (Part.relative(_)) |
    succ(Part.empty))

  val hier_part = (
    ("//" :^ authority ^ path_abempty) -> { case authority ^ path => Part.network(authority, path) } |
    path_absolute -> (Part.absolute(_)) |
    path_rootless -> (Part.relative(_)) |
    succ(Part.empty))

  val uri = (scheme ^ ":" :^ hier_part ^ ("?" :^ query).? ^ ("#" :^ fragment).?) -> (
    as[String] and as[Part] and as[String].default("") and as[String].default("") `then` UriReference.apply)

  val relative_ref = (relative_part ^ ("?" :^ query).? ^ ("#" :^ fragment).?) -> (
    as[Part] and as[String].default("") and as[String].default("") `then` UriReference.apply)

  val absolute_uri = (scheme ^ ":" :^ hier_part ^ ("?" :^ query).?)

  val uri_reference = uri | relative_ref
}