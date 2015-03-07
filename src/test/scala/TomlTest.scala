import com.leacox.toml.TomlAst.{TDouble, TInteger, TTable}
import com.leacox.toml.TomlParser

import org.scalatest.{FunSpec, ShouldMatchers}

/**
 * @author John Leacox
 */
class TomlTest extends FunSpec with ShouldMatchers {
  describe("TomlParser") {
    it("Should fail to parse weird stuff") {
      val message = intercept[RuntimeException] {
        TomlParser.parse("This is not actually toml?")
      }.getMessage

      System.out.println(message)
    }

    it("should parse this one?") {
      val table = TomlParser.parse("key = 23").asInstanceOf[TTable]

      table.get("key") should be(TInteger(23))
    }

    it("should do a whole table") {
      val table = TomlParser.parse("number = 23\nstring = \"My String\"").asInstanceOf[TTable]

      System.out.println(table)
    }

    it("should work with sub-tables") {
      val table = TomlParser.parse("number = 23\nstring = \"My String\"\n[MyTable]\nnumber = 42")

      System.out.println(table)
    }

    it("should work with a second sub-table") {
      val table = TomlParser.parse(
        "number = 23\nstring = \"My String\"\n[MyTable]\nnumber = 42\n[MyOtherTable]\nstring = \"My Other String\"")

      System.out.println(table.children)
    }

    it("should work with a embedded sub-tables") {
      val table = TomlParser.parse(
        "number = 23\nstring = \"My String\"\n[MyTable]\nnumber = 42\n[MyOtherTable]\nstring = \"My Other String\"\n[MyTable.Embedded]\nembeddedString = \"Hello\"")

      System.out.println(table)
    }

    describe("integer") {
      it("should parse +99") {
        val toml = TomlParser.parse("number = +99").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(99)
        }
      }

      it("should parse 42") {
        val toml = TomlParser.parse("number = 42").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(42)
        }
      }

      it("should parse 0") {
        val toml = TomlParser.parse("number = 0").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(0)
        }
      }

      it("should parse -17") {
        val toml = TomlParser.parse("number = -17").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(-17)
        }
      }

      it("should parse 1_000") {
        val toml = TomlParser.parse("number = 1_000").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(1000)
        }
      }

      it("should parse 5_349_221") {
        val toml = TomlParser.parse("number = 5_349_221").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(5349221)
        }
      }

      it("should parse 1_2_3_4_5") {
        val toml = TomlParser.parse("number = 1_2_3_4_5").asInstanceOf[TTable]

        toml.get("number") match {
          case TInteger(x) => x should be(12345)
        }
      }
    }

    describe("float") {
      it("should parse  +1.0") {
        val toml = TomlParser.parse("number = +1.0").asInstanceOf[TTable]

        toml.get("number") match {
          case TDouble(x) => x should be(1.0 +- 0.2)
        }
      }

      it("should parse 3.14") {
        val toml = TomlParser.parse("number = 3.1415").asInstanceOf[TTable]

        toml.get("number") match {
          case TDouble(x) => x should be(3.1415 +- 0.2)
        }
      }

      it("should parse  -0.01") {
        val toml = TomlParser.parse("number = -0.01").asInstanceOf[TTable]

        toml.get("number") match {
          case TDouble(x) => x should be(-0.01 +- 0.2)
        }
      }

      it("should parse  5e+22") {
        val toml = TomlParser.parse("number = 5e+22").asInstanceOf[TTable]

        toml.get("number") match {
          case TDouble(x) => x should be(5e+22 +- 0.2)
        }
      }
    }
  }
}
