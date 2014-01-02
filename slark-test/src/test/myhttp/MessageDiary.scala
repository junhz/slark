package test
package myhttp

import MyHttp._
import parsers._

object MessageDiary extends Diary {

  val content = 
    Source(http_uri parse "http://www.google.com/search?q=rfc+uri&ie=utf-8&oe=utf-8&aq=t&rls=org.mozilla:en-US:official&client=firefox-a") :: 
    Source(request parse """GET http://www.google.com/ HTTP/1.1
Accept: image/gif, image/jpeg, image/pjpeg, image/pjpeg, application/x-shockwave-flash, application/xaml+xml, application/vnd.ms-xpsdocument, application/x-ms-xbap, application/x-ms-application, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*
Accept-Language: en-us
User-Agent: Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 1.0.3705; .NET CLR 1.1.4322; .NET CLR 2.0.50727; .NET CLR 3.0.04506.30; .NET CLR 3.0.04506.648; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; MS-RTC LM 8; InfoPath.3)
Accept-Encoding: gzip, deflate
Proxy-Connection: Keep-Alive
Host: www.google.com
Cookie: PREF=ID=5d7939c828752d5f:U=a9828f5f0d459770:FF=0:TM=1367997130:LM=1384307618:S=VjMOHfPBFOtZvF7S; NID=67=Sw4mT3hAPdL2A-NqrEIiNFxh_RbZauiwKtCy2_CJl5kG2nMe9qHpSACUZhJg0Wh1GV2ohUY3wJ-vLGl9g6-9WOje6Mc89wwwVH-viBKfZJxLnv8Jh3jxDTPm8tdTz-lEyyWsxA

""") :: 
    Source(response parse """HTTP/1.1 302 Found
Location: https://www.google.com/
Cache-Control: private
Content-Type: text/html; charset=UTF-8
Date: Mon, 25 Nov 2013 07:22:09 GMT
Server: gws
X-XSS-Protection: 1; mode=block
X-Frame-Options: SAMEORIGIN
Alternate-Protocol: 80:quic
Content-Length: 220
Via: 1.1 aojpkkpx01a.statestr.com:80 (IronPort-WSA/7.1.3-021), 1.1 aocnhipx01a.statestr.com:80 (IronPort-WSA/7.1.1-038)
Connection: keep-alive
Proxy-Connection: keep-alive

""") :: Nil

}