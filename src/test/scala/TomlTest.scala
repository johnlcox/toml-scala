import com.leacox.toml.TomlAst.{TInteger, TTable}
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

      table.get("key") should be(Some(TInteger(23)))
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
  }
}
