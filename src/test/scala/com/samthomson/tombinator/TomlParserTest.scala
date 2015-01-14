package com.samthomson.tombinator

import javax.xml.bind.DatatypeConverter.parseDateTime

import com.samthomson.tombinator.TomlParser.Assignment
import org.parboiled2.ParseError
import org.scalatest.{FlatSpec, Matchers}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success}

/**
 * Tests for TomlParser
 * Most tests are adapted straight from the TOML spec
 */
class TomlParserTest extends FlatSpec with Matchers {
  "TomlParser.eol" should "parse end of lines" in {
    new TomlParser("# comment \n").eol.run() should be (Success(()))
    new TomlParser("#more comments\t\r").eol.run() should be (Success(()))
  }

  "TomlParser.wsOrComments" should "parse whitespace + comments" in {
    val commentsAndWs = "\t# comment\n   # more comments\r\t "
    val parser = new TomlParser(commentsAndWs)
    parser.wsOrComments.run() should be (Success(()))
    parser.cursor should be (commentsAndWs.size)
  }

  "TomlParser.primitive" should "parse numbers" in {
    val result1 = new TomlParser("5\t ").value.run()
    result1 should be (Success(TomlLong(5)))
    val result2 = new TomlParser("5.7e-23 ").value.run()
    result2 should be (Success(TomlDouble(5.7e-23)))
  }

  "TomlParser.datetime" should "parse ISO 8601 date times" in {
    val validDateStrings = Seq(
      // from RFC 3339
      "1985-04-12T23:20:50.52Z",
      "1996-12-19T16:39:57-08:00",
      "1990-12-31T23:59:60Z",
      "1990-12-31T15:59:60-08:00",
      "1937-01-01T12:00:27.87+00:20",
      // from TOML spec
      "1979-05-27T07:32:00Z",
      "1979-05-27T00:32:00-07:00",
      "1979-05-27T00:32:00.999999-07:00"
    )
    for (dateString <- validDateStrings) {
      new TomlParser(dateString).datetime.run() shouldBe an[Success[_]]
    }
    val invalidDateStrings = Seq(
      "1990-12-31T15:59:60-0",
      "85-04-12T23:20:50.52Z",
      "1937-01-01",
      "1990-12-31T15:59:60"
    )
    for (dateString <- invalidDateStrings) {
      new TomlParser(dateString).datetime.run() shouldBe an[Failure[_]]
    }
  }

  "TomlParser.basicString" should "not allow line breaks" in {
    new TomlParser("\" \n  \"").basicString.run() shouldBe an[Failure[_]]
    new TomlParser("\" \\n \"").basicString.run() should be (Success(TomlString(" \n ")))
  }
  it should "allow only allow escaped double quotes" in {
    new TomlParser("\" \\\" \"").basicString.run() should be (Success(TomlString(" \" ")))
    // string should close at first unescaped `"`
    val parser: TomlParser = new TomlParser("\" \"   \"")
    parser.basicString.run() should be (Success(TomlString(" ")))
    parser.cursor should be (3)
  }
  it should "allow escaped or unescaped single quotes" in {
    new TomlParser("\" \'   \"").basicString.run() should be (Success(TomlString(" \'   ")))
    new TomlParser("\" \\\' \"").basicString.run() should be (Success(TomlString(" \' ")))
  }
  it should "handle 8-digit unicode escapes" in {
    val parser = new TomlParser("\"\\U0010ffff\"")
    val result = parser.basicString.run()
    result should be (Success(TomlString("\uDBFF\uDFFF")))
  }

  "TomlParser.literalString" should "not allow line breaks" in {
    new TomlParser("\' \n  \'").literalString.run() shouldBe an[Failure[_]]
    new TomlParser("\' \\n \'").literalString.run() should be (Success(TomlString(" \\n ")))
  }
  it should "not allow single quotes" in {
    // string should close at first unescaped `'`
    val parser: TomlParser = new TomlParser("\' \'   \'")
    parser.literalString.run() should be (Success(TomlString(" ")))
    parser.cursor should be (3)
  }
  it should "pass through backslashes" in {
    new TomlParser("\' \\\"   \'").literalString.run() should be (Success(TomlString(" \\\"   ")))
    new TomlParser("\' \\t \t \'").literalString.run() should be (Success(TomlString(" \\t \t ")))
  }

  "TomlParser.multiLineBasicString" should "allow line breaks" in {
    new TomlParser("\"\"\" \n  \"\"\"").multiLineBasicString.run() should be (Success(TomlString(" \n  ")))
    new TomlParser("\"\"\" \\n \"\"\"").multiLineBasicString.run() should be (Success(TomlString(" \n ")))
  }
  it should "allow escaped or unescaped quotes" in {
    new TomlParser("\"\"\" \"   \"\"\"").multiLineBasicString.run() should be (Success(TomlString(" \"   ")))
    new TomlParser("\"\"\" \\\" \"\"\"").multiLineBasicString.run() should be (Success(TomlString(" \" ")))
    new TomlParser("\"\"\" \'   \"\"\"").multiLineBasicString.run() should be (Success(TomlString(" \'   ")))
    new TomlParser("\"\"\" \\\' \"\"\"").multiLineBasicString.run() should be (Success(TomlString(" \' ")))
  }
  it should "trim whitespace when a line ends in backslash" in {
    val expected = Success(TomlString("\"The quick brown fox jumps over the lazy dog.\""))
    val input1 = "\"\"\"\n\"The quick brown \\\n\n\n  fox jumps over \\\n    the lazy dog.\\\"\"\"\""
    val parser1 = new TomlParser(input1)
    val result1 = parser1.multiLineBasicString.run()
    result1 should be (expected)
    parser1.cursor should equal (input1.size)
    val input2 = "\"\"\"\"\\\n       The quick brown \\\n       fox jumps over \\\n       the lazy dog.\"\\\n       \"\"\""
    val parser2 = new TomlParser(input2)
    val result2 = parser2.multiLineBasicString.run()
    result2 should be (expected)
    parser2.cursor should equal (input2.size)
  }
  "TomlParser.trimmedWs" should "trim whitespace when a line ends in backslash" in {
    val input = "\\\n\n\n \t asdf"
    val parser = new TomlParser(input)
    parser.trimmedWs.run() should be (Success(()))
    parser.cursor should be (7)
  }

  "TomlParser.multiLineLiteralString" should "not escape anything" in {
    val input = "'''I [dw]on't need \\d{2} apples'''"
    val expected = Success(TomlString("I [dw]on't need \\d{2} apples"))
    new TomlParser(input).multiLineLiteralString.run() should be (expected)
  }
  it should "trim one initial line break" in {
    val input = "'''\n\nThe first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n'''"
    val expected = Success(TomlString("\nThe first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n"))
    new TomlParser(input).multiLineLiteralString.run() should be (expected)
  }

  "TomlParser.statement" should "parse assignments" in {
    val validAssignments = Seq(
      ("foo =\t5.2\t# comment",             Assignment("foo", TomlDouble(5.2))),
      ("bar\t= 5 #\tcomment\n#\tcomment\n", Assignment("bar", TomlLong(5))),
      ("bar\t= 5",                          Assignment("bar", TomlLong(5)))
    )
    for ((assnmentStr, expected) <- validAssignments) {
      val result = new TomlParser(assnmentStr).statement.run()
      result should be (Success(expected))
    }
  }
  it should "not allow new lines" in {
    val result = new TomlParser("bar\n= 5 #\tcomment\n").statement.run()
    result shouldBe an[Failure[_]]
  }
  // FULL DOCUMENTS
  "A TomlParser" should "parse an empty TOML doc" in {
    val result = new TomlParser("").toml.run()
    result should be (Success(TomlTable(Map())))
  }
  it should "parse a commented-out TOML doc" in {
    val commentedOut = """# # # whole file is commented out
      |# title = "TOML Example"
      |#
      |# [owner]"""".stripMargin
    val result = new TomlParser(commentedOut).toml.run()
    result should be (Success(TomlTable(Map())))
  }
  it should "parse a single assignment" in {
    val parser = new TomlParser("\n foo =\t5.2\t# comment")
    val result = parser.toml.run()
    result match {
      case Failure(e: ParseError) =>
        System.err.println(parser.formatError(e))
        System.err.println(e.formatTraces)
      case _ => ()
    }
    result should be (Success(TomlTable(Map("foo" -> TomlDouble(5.2)))))
  }
  it should "parse an empty table" in {
    val input = "[a]"
    val parser = new TomlParser(input)
    val result = parser.toml.run()
    result shouldBe an[Success[_]]
    result.get.value should be (Map("a" -> Map()))
  }
  it should "parse an empty array of tables" in {
    val input = "[[a]]"
    val result = new TomlParser(input).toml.run()
    result shouldBe an[Success[_]]
    result.get.value should be (Map("a" -> List(Map())))
  }
  it should "autovivify uncles" in {
    val input = "[a.b]\nc = 1\n\n[a]\nd = 2"
    val expected = Map(
      "a" -> Map(
        "b" -> Map(
          "c" -> 1
        ),
        "d" -> 2
      )
    )
    val result = new TomlParser(input).toml.run()
    result shouldBe an[Success[_]]
    result.get.value should be (expected)
  }
//  // TOML specifies this, but I'm ignoring it for now
//  it should "not allow the same key group in multiple places" in {
//    val input = "[a]\nb = 1\n\n[a]\nc = 2"
//    new TomlParser(input).toml.run() shouldBe an[Failure[_]]
//  }
  it should "not allow duplicate keys" in {
    val input =
      """[a]
        |b = 1
        |
        |[a.b]
        |c = 2""".stripMargin
    new TomlParser(input).toml.run() shouldBe an[Failure[_]]
  }
  it should "not allow empty keys" in {
    val inputs = Seq(
      "[]",
      "[a.]",
      "[a..b]",
      "[.b]",
      "[.]",
      " = \"no key name\" # not allowed"
    )
    for (input <- inputs) {
      new TomlParser(input).toml.run() shouldBe an[Failure[_]]
    }
  }
  it should "parse a simple array of tables" in {
    val input =
      """[[products]]
        |name = "Hammer"
        |sku = 738594937
        |
        |[[products]]
        |
        |[[products]]
        |name = "Nail"
        |sku = 284758393
        |color = "gray"""".stripMargin
    val expected = Map(
      "products" -> List(
        Map(
          "name" -> "Hammer",
          "sku" -> 738594937
        ),
        Map(),
        Map(
          "name" -> "Nail",
          "sku" -> 284758393,
          "color" -> "gray"
        )
      )
    )
    val result = new TomlParser(input).toml.run()
    result shouldBe an[Success[_]]
    result.get.value should be (expected)
  }
  it should "parse nested arrays of tables" in {
    val input =
      """[[fruit]]
        |  name = "apple"
        |
        |  [fruit.physical]
        |    color = "red"
        |    shape = "round"
        |
        |  [[fruit.variety]]
        |    name = "red delicious"
        |
        |  [[fruit.variety]]
        |    name = "granny smith"
        |
        |[[fruit]]
        |  name = "banana"
        |
        |  [[fruit.variety]]
        |    name = "plantain"""".stripMargin
    val expected = Map(
      "fruit" -> List(
        Map(
          "name" -> "apple",
          "physical" -> Map(
            "color" -> "red",
            "shape" -> "round"
          ),
          "variety" -> List(
            Map("name" -> "red delicious"),
            Map("name" -> "granny smith")
          )
        ),
        Map(
          "name" -> "banana",
          "variety" -> List(
            Map("name" -> "plantain")
          )
        )
      )
    )
    val parser = new TomlParser(input)
    val result = parser.toml.run()
//    println(input.substring(0, parser.cursor) + "^")
    result shouldBe an[Success[_]]
    result.get.value should be (expected)
  }
  it should "not allow a table and an array of tables under the same key" in {
    val input =
      """[[fruit]]
        |  name = "apple"
        |
        |  [[fruit.variety]]
        |    name = "red delicious"
        |
        |  # This table conflicts with the previous array of tables
        |  [fruit.variety]
        |    name = "granny smith"""".stripMargin
    new TomlParser(input).toml.run() shouldBe an[Failure[_]]
  }
  it should "parse a sample file" in {
    val example = readTestFile("/example.toml")
    val expected = Map(
      "title" -> "TOML Example",
      "basic_string" -> "I'm a string. '\"You can quote me\"'. Name\tJosé\nLocation\tSF. Tab \t newline \n you get \\ it.",
      "literal_string" -> "Im a string. \"You can quote me\\\". Name\\tJosé\\u00E9\\nLocation\\tSF. Tab \\t newline \\n you get \\\\ it.",
      "multi_line_basic" -> "I'm a string.  '\"You can quote me\"'. Tab \t newline\n you get \\ it.",
      "multi_line_literal_string" -> "I'm a string. '\"You can quote me\"'. Name José\\u00E9\nLocation    SF. Tab      newline\n you get \\ it.",
      "owner" -> Map(
        "name" -> "Tom Preston-Werner",
        "organization" -> "GitHub",
        "bio" -> "GitHub Cofounder & CEO\nLikes tater tots and beer.",
        "dob" -> parseDateTime("1979-05-27T07:32:00Z").getTime
      ),
      "database" -> Map(
        "server" -> "192.168.1.1",
        "ports" -> List(8001, 8001, 80020),
        "onearr" -> List(1),
        "empty" -> List(),
        "twoArr" -> List("a", "ac"),
        "connection_max" -> 5000,
        "enabled" -> true
      ),
      "servers" -> Map(
        "alpha" -> Map(
          "ip" -> "10.0.0.1",
          "dc" -> "eqdc10"
        ),
        "beta" -> Map(
          "ip" -> "10.0.0.2",
          "dc" -> "eqdc10"
        )
      ),
      "clients" -> Map(
        "data" -> List(
          List("gamma", "delta"),
          List(1, 2)
        )
      )
    )
    val parser = new TomlParser(example)
    val result = parser.toml.run()
    result shouldBe an[Success[_]]
//    def recursiveEq(a: Map[String, Any], b: Map[String, Any]): Boolean = {
//      if (a.keys.toSet != b.keys.toSet) {
//        println("different keys:\n" + a.keys.toSet + "\n" + b.keys.toSet)
//        false
//      } else {
//        a.keys forall { key =>
//          (a(key), b(key)) match {
//            case (av: Map[String, Any], bv: Map[String, Any]) => recursiveEq(av, bv)
//            case (av, bv) => if (av != bv) {
//              println("diff:\n" + av + "\n" + bv)
//              false
//            } else true
//          }
//        }
//      }
//    }
//    recursiveEq(result.get.value, expected)
    result.get.value should equal (expected)
  }

  def readTestFile(filename: String): String = {
    val inputStream = getClass.getResourceAsStream(filename)
    Source.fromInputStream(inputStream)(Codec.UTF8).mkString
  }

  it should "parse the valid BurntSushi files" in {
    val validFilenames = readTestFile("/burntsushi-toml-test/valid/filenames.txt").split('\n')
    for (filename <- validFilenames) {
      println(filename)
      val input = readTestFile("/burntsushi-toml-test/valid/" + filename + ".toml")
      val parser = new TomlParser(input)
      val result = parser.toml.run()
      result match {
        case Failure(e: ParseError) =>
          System.err.println(parser.formatError(e))
          System.err.println(e.formatTraces)
        case _ => ()
      }
      //TODO: check that it matches json
      result shouldBe an[Success[_]]
    }
  }
//  // FIXME: need to check that arrays are homogenous
//  it should "fail to parse the invalid BurntSushi files" in {
//    val invalidFilenames = readTestFile("/burntsushi-toml-test/invalid/filenames.txt").split('\n')
//    for (filename <- invalidFilenames) {
//      val input = readTestFile("/burntsushi-toml-test/invalid/" + filename + ".toml")
//      val parser = new TomlParser(input)
//      val result = parser.toml.run()
//      result shouldBe an[Failure[_]]
//    }
//  }
}
