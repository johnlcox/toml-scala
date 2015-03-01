import com.leacox.toml.TomlAst.TTable
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

      table.get("key") should be(Some(23))
    }

    it("should match x :: nil") {
      List(1) match {
        case x :: xs => System.out.println("Yes!")
        case x :: Nil => System.out.println("no :(")
      }
    }
  }
}
