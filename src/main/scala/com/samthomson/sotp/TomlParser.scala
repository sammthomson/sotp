package com.samthomson.sotp

import javax.xml.bind.DatatypeConverter.parseDateTime
import com.samthomson.sotp.TomlValue.DuplicateKeyException
import org.parboiled2._
import shapeless.{HNil, :: => :::}

object TomlParser {

  sealed trait Statement

  case class Assignment[V](key: String, value: TomlValue[V]) extends Statement

  sealed abstract class KeyPath(val keys: Seq[String], val default: TomlContainer) extends Statement
  case class ArrayKeyPath(override val keys: Seq[String]) extends KeyPath(keys, TomlArray.empty)
  case class TableKeyPath(override val keys: Seq[String]) extends KeyPath(keys, TomlTable.empty)

  case class State(table: TomlTable, path: KeyPath, pathsUsed: Set[Seq[String]]) {
    def update(stmt: Statement): State = stmt match {
      case keyPath: KeyPath =>
        val newTableKeyPath = Some(keyPath).collect { case tkp: TableKeyPath =>
          // TOML says it's OK if a table already exists b/c of
          // auto-vivification, but not OK if it was b/c it was explicitly
          // specified previously.
          // So we keep track of all explicitly specified table paths.
          if (pathsUsed.contains(tkp.keys))
            throw DuplicateKeyException(tkp.keys)
          tkp.keys
        }
        State(
          table.addKeyPath(keyPath, keyPath.keys),
          keyPath,
          pathsUsed ++ newTableKeyPath
        )
      case Assignment(ident, value) =>
        copy(table = table.assign(path.keys, ident, value))
    }
  }
  object State {
    val start: State = State(TomlTable.empty, TableKeyPath(Seq()), Set())
  }
}

class TomlParser(val input: ParserInput) extends Parser with StringBuilding {
  import org.parboiled2.CharPredicate.{Digit, Digit19, HexDigit}
  import TomlParser._

  // Root rule to parse a whole doc
  def toml: Rule1[TomlTable] = rule {
    push(State.start) ~ wsOrComments ~ processStatements ~ EOI ~> ((_: State).table)
  }

  def processStatements: Rule[State ::: HNil, State ::: HNil] = rule {
    (statement ~> ((_: State) update _)).*
  }

  //********* top-level expressions *********
  def statement: Rule1[Statement] = rule {
    (tableArrayKeyPath | tableKeyPath | assignment) ~ eol ~!~ wsOrComments
  }
  def assignment: Rule1[Assignment[Any]] = rule {
    ident ~ nonNlWs ~ "=" ~!~ nonNlWs ~ value ~!~ nonNlWs ~> Assignment.apply[Any] _
  }
  def tableArrayKeyPath: Rule1[ArrayKeyPath] = rule {
    "[[" ~ nonNlWs ~ ident.+.sep('.') ~ nonNlWs ~ "]]" ~!~ nonNlWs ~> ArrayKeyPath
  }
  def tableKeyPath: Rule1[TableKeyPath] = rule {
    "[" ~ nonNlWs ~ ident.+.sep('.') ~ nonNlWs ~ "]" ~!~ nonNlWs ~> TableKeyPath
  }
  // Latest tag (v0.3.1) says:
  // Name your tables whatever crap you please, just don't use #, ., [ or ].
  def ident: Rule1[String] = rule {
    capture(noneOf("=#.[]" ++ wsChars).+.sep(nonNlWs.?))
  }
  // But...
  // HEAD as of 2014-01-13 (a5f2d76fe6a55146f9ac301a140123ba1ca93b24) says:
  // Identifiers may only consist of non-whitespace, non-newline characters
  // excluding '=', '#', '.', '[', and ']'
  //def ident: Rule1[String] = rule { capture(noneOf("=#.[]" ++ wsChars).+) }

  //******** value types *********
  def sign: Rule0 = rule { anyOf("+-") }
  def signedInteger: Rule0 = rule {
    // no leading '0's allowed
    sign.? ~ ('0' | (Digit19 ~ Digit.*))
  }
  def long: Rule1[TomlLong] = rule {
    capture(signedInteger) ~> ((s: String) => TomlLong(s.toLong))
  }

  def decimalPart: Rule0 = rule { '.' ~ Digit.+ }
  def expd: Rule0 = rule { ignoreCase('e') ~ signedInteger }
  def double: Rule1[TomlDouble] = rule {
    capture(signedInteger ~ (decimalPart ~ expd | decimalPart | expd)) ~> (
      (s: String) => TomlDouble(s.toDouble)
    )
  }

  def bool: Rule1[TomlBool] = rule {
    "true" ~ push(True) | "false" ~ push(False)
  }

  // ****** strings *********
  // it's important that the multi-line versions come before their
  // single-line variants
  def string: Rule1[TomlString] = rule {
    multiLineBasicString | basicString | multiLineLiteralString | literalString
  }
  // these are all unambiguous (as long as multi-line versions come first), so
  // I sprinkled cuts around to avoid backtracking
  def multiLineBasicString: Rule1[TomlString] = rule {
    ("\"\"\"" ~ clearSB ~!~ '\n'.? ~
      ((trimmedWs
        | escapedChar
        | (!"\"\"\"" ~ ANY ~ appendSB)
        ) ~!~ MATCH).* ~!~
      "\"\"\""
      ) ~ push(TomlString(sb.toString))
  }
  def trimmedWs: Rule0 = rule { '\\' ~ newLine ~!~ anyOf(wsChars).* }
  def basicString: Rule1[TomlString] = rule {
    ('\"' ~ clearSB ~!~
      (escapedChar ~!~ MATCH
        | (noneOf(newLineChars + "\"") ~!~ appendSB)
      ).* ~
      '\"') ~ push(TomlString(sb.toString))
  }
  def multiLineLiteralString: Rule1[TomlString] = rule {
    ("'''" ~!~ '\n'.? ~
      capture((!"'''" ~ ANY ~!~ MATCH).*) ~
      "'''") ~> TomlString
  }
  def literalString: Rule1[TomlString] = rule {
    ('\'' ~
      capture((noneOf(newLineChars + '\'') ~!~ MATCH).*) ~!~
      '\'') ~> TomlString
  }
  def escapedChar: Rule0 = rule { '\\' ~!~ (
      (anyOf("\"'\\/") ~ appendSB)
        | 'b' ~ appendSB('\b')
        | 'f' ~ appendSB('\f')
        | 'n' ~ appendSB('\n')
        | 'r' ~ appendSB('\r')
        | 't' ~ appendSB('\t')
        | unicode
    )
  }
  def unicode: Rule0 = rule {
    ('u' ~!~ capture(4 times HexDigit)
      | 'U' ~!~ capture(8 times HexDigit)) ~> (
      (s: String) => appendSB(unicodeCodePointToStr(s)))
  }
  def unicodeCodePointToStr(s: String): String = {
    // this inefficient, but easiest way to do it correctly.
    val codePoint = BigInt(s, 16) // convert from hex
    // convert to big-endian 4-byte array (left-padding with 0 bytes if necessary)
    val bytes = codePoint.toByteArray.reverse.padTo(4, 0: Byte).reverse
    // which is exactly what UTF-32BE needs (BE = big-endian)
    new String(bytes, "UTF-32BE") // back to JVM's UTF-16
  }

  // Datetimes are RFC 3339 / ISO 8601 dates
  def twoDigit: Rule0 = rule { 2 times Digit }
  def date: Rule0 = rule { (4 times Digit) ~ '-' ~ twoDigit ~ '-' ~ twoDigit }
  def timeOffset: Rule0 = rule {
    'Z' |  (sign ~ twoDigit ~ ':' ~ twoDigit)
  }
  def time: Rule0 = rule {
    // offset is mandatory
    twoDigit ~ ':' ~ twoDigit ~ ':' ~ twoDigit ~ decimalPart.? ~ timeOffset
  }
  def datetime: Rule1[TomlDateTime] = rule {
    capture(date ~ 'T' ~ time) ~> (
      (s: String) => TomlDateTime(parseDateTime(s).getTime))
  }

  // TODO: Check that types of all elements match (up to erasure)
  def array: Rule1[TomlArray[Any]] = rule {
    // `array`s may contain line breaks
    ('[' ~ wsOrComments ~ ']' ~ push(TomlArray[Any](Seq()))  // empty array, can't contain trailing ','
    | (
      '[' ~
      (wsOrComments ~ value ~ wsOrComments).+.sep(',') ~
      (',' ~ wsOrComments).? ~ // non-empty array, can contain trailing ','
      ']' ~> (TomlArray[Any] _)
      )
    )
  }

  // `long` is a prefix of `datetime` and `double`, so it's important that
  // it come after them in the short-circuiting disjunction
  def value: Rule1[TomlValue[Any]] = rule {
    (datetime | double | long | bool | array | string) ~!~ nonNlWs
  }

  // ****** whitespace ***********
  val newLineChars = "\n\r\f"
  val nonNlWsChars = " \t"
  val wsChars = nonNlWsChars ++ newLineChars
  def nonNlWs: Rule0 = rule { anyOf(nonNlWsChars).* }
  def newLine: Rule0 = rule { anyOf(newLineChars) }
  def eol: Rule0 = rule {
    ('#' ~ noneOf(newLineChars).*).? ~ &(newLine | EOI)
  }
  // Matches any contiguous series of whitespace (including newlines),
  // and/or comments. Stops just short of EOI.
  def wsOrComments: Rule0 = rule { (nonNlWs ~ eol.?).*.sep(newLine) ~!~ MATCH }
}
