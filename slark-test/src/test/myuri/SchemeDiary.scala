package test
package myuri

import MyScheme._
import parsers._

object SchemeDiary extends Diary {
  
  val content = Source(dec_octet parse "1") ::
//decOctet
    Source(dec_octet parse "11") :: 
    Source(dec_octet parse "111") :: 
    Source(dec_octet parse "211") :: 
    Source(dec_octet parse "251") :: 
    Source(dec_octet parse "256") :: 
//pchar
    Source(pchar parse "a") :: 
    Source(pchar parse "#") :: 
//h16
    Source(h16 parse "1111") :: 
    Source(h16 parse "111") :: 
    Source(h16 parse "11") :: 
    Source(h16 parse "1") :: 
    Source(h16 parse "11111") :: 
    Source(h16 parse "") :: 
//segment
    Source(segment_nz parse "") :: 
    Source(segment_nz parse ":") :: 
    Source(segment_nz parse ":#") :: 
    Source(segment_nz parse "%25%E7%8C%98") :: 
    Source(ipv4address parse "127.0.0.1") :: 
    Source(IPv6address.compressed(1 :: 2 :: 3 :: 4 :: Nil, 5 :: 6 :: 7 :: 8 :: Nil)) :: 
// ipv61
    Source(ipv6address parse "0:0:0:0:0:0:0:0") :: 
    Source(ipv6address parse "0:0:0:0:0:0:127.0.0.1") :: 
// ipv62
    Source(ipv6address parse "::0:0:0:0:0:0:0") :: 
    Source(ipv6address parse "::0:0:0:0:0:127.0.0.1") :: 
// ipv63
    Source(ipv6address parse "::0:0:0:0:0:0") :: 
    Source(ipv6address parse "::0:0:0:0:127.0.0.1") :: 
    Source(ipv6address parse "0::0:0:0:0:0:0") :: 
    Source(ipv6address parse "0::0:0:0:0:127.0.0.1") :: 
// ipv64
    Source(ipv6address parse "::0:0:0:0:0") :: 
    Source(ipv6address parse "::0:0:0:127.0.0.1") :: 
    Source(ipv6address parse "0::0:0:0:0:0") :: 
    Source(ipv6address parse "0::0:0:0:127.0.0.1") :: 
    Source(ipv6address parse "0:0::0:0:0:0:0") :: 
    Source(ipv6address parse "0:0::0:0:0:127.0.0.1") :: 
// ipv65
    Source(ipv6address parse "::0:0:0:0") :: 
    Source(ipv6address parse "::0:0:127.0.0.1") :: 
    Source(ipv6address parse "0::0:0:0:0") :: 
    Source(ipv6address parse "0::0:0:127.0.0.1") :: 
    Source(ipv6address parse "0:0::0:0:0:0") :: 
    Source(ipv6address parse "0:0::0:0:127.0.0.1") :: 
    Source(ipv6address parse "0:0:0::0:0:0:0") :: 
    Source(ipv6address parse "0:0:0::0:0:127.0.0.1") :: 
// ipv66
    Source(ipv6address parse "::0:0:0") :: 
    Source(ipv6address parse "::0:127.0.0.1") :: 
    Source(ipv6address parse "0::0:0:0") :: 
    Source(ipv6address parse "0::0:127.0.0.1") :: 
    Source(ipv6address parse "0:0::0:0:0") :: 
    Source(ipv6address parse "0:0::0:127.0.0.1") :: 
    Source(ipv6address parse "0:0:0::0:0:0") :: 
    Source(ipv6address parse "0:0:0::0:127.0.0.1") :: 
    Source(ipv6address parse "0:0:0:0::0:0:0") :: 
    Source(ipv6address parse "0:0:0:0::0:127.0.0.1") :: 
// ipv67
    Source(ipv6address parse "::0:0") :: 
    Source(ipv6address parse "::127.0.0.1") :: 
    Source(ipv6address parse "0::0:0") :: 
    Source(ipv6address parse "0::127.0.0.1") :: 
    Source(ipv6address parse "0:0::0:0") :: 
    Source(ipv6address parse "0:0::127.0.0.1") :: 
    Source(ipv6address parse "0:0:0::0:0") :: 
    Source(ipv6address parse "0:0:0::127.0.0.1") :: 
    Source(ipv6address parse "0:0:0:0::0:0") :: 
    Source(ipv6address parse "0:0:0:0::127.0.0.1") :: 
    Source(ipv6address parse "0:0:0:0:0::0:0") :: 
    Source(ipv6address parse "0:0:0:0:0::127.0.0.1") :: 
// ipv67
    Source(ipv6address parse "::0") :: 
    Source(ipv6address parse "0::0") :: 
    Source(ipv6address parse "0:0::0") :: 
    Source(ipv6address parse "0:0:0::0") :: 
    Source(ipv6address parse "0:0:0:0::0") :: 
    Source(ipv6address parse "0:0:0:0:0::0") :: 
    Source(ipv6address parse "0:0:0:0:0:0::0") :: 
// ipv68
    Source(ipv6address parse "::") :: 
    Source(ipv6address parse "0::") :: 
    Source(ipv6address parse "0:0::") :: 
    Source(ipv6address parse "0:0:0::") :: 
    Source(ipv6address parse "0:0:0:0::") :: 
    Source(ipv6address parse "0:0:0:0:0::") :: 
    Source(ipv6address parse "0:0:0:0:0:0::") :: 
    Source(ipv6address parse "0:0:0:0:0:0:0::") :: 
// scheme
    Source(authority parse "www.google.com") :: 
    Source(query parse "q=rfc+uri&ie=utf-8&oe=utf-8&aq=t&rls=org.mozilla:en-US:official&client=firefox-a") :: 
    Source(path_abempty parse "/search") :: 
    Source(hier_part parse "//www.google.com/search") :: 
// uri
    Source(uri parse "my://www.google.com/search?q=rfc+uri&ie=utf-8&oe=utf-8&aq=t&rls=org.mozilla:en-US:official&client=firefox-a") :: 
    Source(uri parse "my://user:password@www.my.com/") :: 
    Source(uri parse "who://www.my.com/") :: Nil

}