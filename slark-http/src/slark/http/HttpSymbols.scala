package slark
package http

import combinator.parser._

abstract class HttpSymbols[CharP <: Parsers with uri.CharReaders, +OctetP <: Parsers with OctetReaders with ImportChars[CharP]] extends Symbols[OctetP] with Literals with Message