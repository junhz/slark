package slark
package http

import combinator.parser._

abstract class HttpSymbols[+P <: Parsers with OctetReaders with ImportChars[Parsers with uri.CharReaders]] extends Symbols[P] with Literals with Message