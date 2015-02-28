package com.leacox.toml

/**
 * @author John Leacox
 */
class Toml {
  def parse(input: TomlInput): TValue = input match {
    case StringInput(s) => TomlParser.parse(s)
  }
}
