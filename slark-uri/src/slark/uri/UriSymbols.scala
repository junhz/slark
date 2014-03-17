package slark
package uri

import combinator.parser._

abstract class UriSymbols[+P <: Parsers with CharReaders] extends Symbols[P] with Literals with IPaddress with Path with Scheme