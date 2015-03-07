package com.leacox.toml

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * @author John Leacox
 */
trait TomlParser extends JavaTokenParsers {
  private sealed trait Statement
  private case class Assignment(key: String, value: TValue) extends Statement
  private case class TableOpen(keys: List[String]) extends Statement
  private case class ArrayOfTablesOpen(keys: List[String]) extends Statement

  def parse(s: String): TValue = parseAll(statement.*, s) match {
    case Success(result, next) => buildTable(result)
    case NoSuccess(message, input) =>
      throw new RuntimeException(message) // TODO: Change exception
  }

  private def buildTable(statements: List[Statement]): TTable = {
    statements.foldLeft((TTable(Map()), List[String]())) {
      case ((table, keyPath), statement) => statement match {
        case Assignment(key, value) =>
          (addForKeyPath(table, keyPath :+ key, value), keyPath)
        case TableOpen(keys) => (table, keys)
      }
    }._1
  }

  private def addForKeyPath(table: TTable, keyPath: List[String],
      value: TValue): TTable = keyPath match {
    //case Nil =>
    case x :: Nil => TTable(table.values.updated(x, value))
    case x :: xs => table.values.getOrElse(x, TTable(Map())) match {
      case childTable: TTable => TTable(
        table.values.updated(x, addForKeyPath(childTable, xs, value)))
      case _ => throw new ParseException(
        s"Duplicate key found: $x") // TODO: The error should really contain the whole keypath
    }
  }

  // Primities
  private val string = stringLiteral ^^ { x => TString(x) }
  // Literal strings?
  // TODO Why is '$' required here vs added to the primitive definition as '<~ "$".r'?
  private val integer = """-?\+?(\d+(\d*_\d*)*\d+|\d+)[^\.]$""".r ^^
      { x => println(s"val is: |$x|"); TInteger(x.toLong) }
  private val float =
    """-?\+?((\d+(\d*_\d*)*\d+|\d+)(\.(\d+(\d*_\d*)*\d+|\d+))?(([eE]-?\+?(\d+(\d*_\d*)*\d+|\d+))?))"""
        .r ^^ { x => TDouble(x.toDouble) }
  private val boolean = Nil
  private val datetime = Nil

  private val keyPart = """(([A-Za-z0-9_-]+)|(\"[A-Za-z0-9_:.,?!@#-]+\"))""".r ^^ { _.toString }
  private val equals = """\s+=\s+""".r

  // TODO: Special cases
  private def openMultilineString = Nil
  private def closeMultilineString = Nil

  private val tableOpen: Parser[TableOpen] = "[" ~> rep1sep(keyPart, ".") <~ "]" ^^
      { x => TableOpen(x) }
  private def closeTable = Nil

  private def openMultilineArray = Nil
  private def closeMultilineArray = Nil

  private def primitive = string | integer | float
  private def assignment: Parser[Assignment] = (keyPart <~ "=") ~ primitive ^^
      { case key ~ value => Assignment(key, value) }
  private def statement = tableOpen | assignment
}

class ParseException(message: String) extends RuntimeException(message)

object TomlParser extends TomlParser