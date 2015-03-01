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
    //      val fields = result.map {
    //        case Assignment(key, value) => TField(key, value)
    //        case TableOpen(keys) => TField(keys(0), TTable())
    //      }
    //      TTable(fields)
    case NoSuccess(message, input) =>
      throw new RuntimeException(message) // TODO: Change exception
  }

  //  private def buildFields(statements: List[Statement]) = {
  //    statements.map {
  //      case Assignment(key, value) => TField(key, value)
  //      case TableOpen(keys) => TField(keys(0), TTable())
  //    }
  //  }

  // TODO: Implement
  private def buildTable(statements: List[Statement]): TTable = {
    statements.foldLeft((TTable(List()), List[String]())) {
      case ((table, keyPath), statement) => statement match {
        case Assignment(key, value) => {
          val assignmentTable = getTableForKeyPath(table, keyPath)
          (assignmentTable.values.updated(key,))
        }
        case TableOpen(keys) => (table, keys)
      }
    }
    TTable(List(TField("key", TNothing)))
  }

  private def getTableForKeyPath(table: TTable, keyPath: List[String]): TTable = keyPath match {
    case Nil => table
    case x :: xs => table.values.getOrElse(x, TTable(List())) match {
      case childTable: TTable => val a = childTable.values + (key -> getTableForKeyPath(childTable, xs))
      case _ => throw new ParseException(
        s"Duplicate key found: $x") // TODO: The error should really contain the whole keypath
    }
  }

  val string = stringLiteral ^^ { x => TString(x) }
  // Literal strings?

  val integer = """-?\+?(\d+(\d*_\d*)*\d+|\d+)""".r ^^ { x => TInteger(x.toLong) }

  val key = """(([A-Za-z0-9_-]+)|(\"[A-Za-z0-9_:.,?!@#-]+\"))""".r ^^ { _.toString }
  val equals = """\s+=\s+""".r

  // TODO: Special cases
  def openMultilineString = Nil
  def closeMultilineString = Nil

  val tableOpen: Parser[TableOpen] = "[" ~> rep1sep(key, ".") <~ "]" ^^ { x => TableOpen(x) }
  def closeTable = Nil

  def openMultilineArray = Nil
  def closeMultilineArray = Nil

  val assignment: Parser[Assignment] = (key <~ "=") ~ (string | integer) ^^
      { case key ~ value => Assignment(key, value) }
  val statement = tableOpen | assignment
}

class ParseException(message: String) extends RuntimeException(message)

object TomlParser extends TomlParser