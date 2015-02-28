package com.leacox.toml

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * @author John Leacox
 */
class TomlParser extends JavaTokenParsers {
  def parse(s: String): TValue =

  val string = stringLiteral // include escaped characters
  // Literal strings?

  val integer = """-?+?\d+""".r

  // TODO: Special cases
  def openMultilineString = Nil
  def closeMultilineString = Nil

  def openTable = Nil
  def closeTable = Nil

  def openMultilineArray = Nil
  def closeMultilineArray = Nil
}
