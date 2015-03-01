package com.leacox.toml

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * @author John Leacox
 */
trait TomlParser extends JavaTokenParsers {
  sealed trait Statement
  case class Assignment(key: String, value: TValue) extends Statement
  case class TableOpen(keys: List[String]) extends Statement
  case class ArrayOfTablesOpen(keys: List[String]) extends Statement

  def parse(s: String): TValue = parseAll(statement.*, s) match {
    case Success(result, next) => buildTable(result)
    case NoSuccess(message, input) =>
      throw new RuntimeException(message) // TODO: Change exception
  }

  private def buildTable(statements: List[Statement]): TTable = {
    statements.foldLeft((TTable(Map()), List[String]())) {
      case ((table, keyPath), statement) => statement match {
        case Assignment(key, value) =>
          (TTable(getTableForKeyPath(table, keyPath).values.updated(key, value)), keyPath)
        case TableOpen(keys) => (table, keys)
      }
    }._1
  }

  private def getTableForKeyPath(table: TTable, keyPath: List[String]): TTable = keyPath match {
    case Nil => table
    case x :: xs => table.values.getOrElse(x, TTable(Map())) match {
      case childTable: TTable => TTable(
        childTable.values.updated(x, getTableForKeyPath(childTable, xs)))
      case _ => throw new ParseException(
        s"Duplicate key found: $x") // TODO: The error should really contain the whole keypath
    }
  }

  val string = stringLiteral ^^ { x => TString(x) }
  // Literal strings?

  val integer = """-?\+?(\d+(\d*_\d*)*\d+|\d+)""".r ^^ { x => TInteger(x.toLong) }

  val keyPart = """(([A-Za-z0-9_-]+)|(\"[A-Za-z0-9_:.,?!@#-]+\"))""".r ^^ { _.toString }
  val equals = """\s+=\s+""".r

  // TODO: Special cases
  def openMultilineString = Nil
  def closeMultilineString = Nil

  val tableOpen: Parser[TableOpen] = "[" ~> rep1sep(keyPart, ".") <~ "]" ^^ { x => TableOpen(x) }
  def closeTable = Nil

  def openMultilineArray = Nil
  def closeMultilineArray = Nil

  val assignment: Parser[Assignment] = (keyPart <~ "=") ~ (string | integer) ^^
      { case key ~ value => Assignment(key, value) }
  def statement = tableOpen | assignment
}

class ParseException(message: String) extends RuntimeException(message)

object TomlParser extends TomlParser