package com.leacox.toml

/**
 * @author John Leacox
 */
trait Toml {
  def parse(input: TomlInput): TValue = input match {
    case StringInput(s) => TomlParser.parse(s)
    case ReaderInput(reader) => TNothing // do something with the reader
    case StreamInput(stream) => TNothing // do something with the stream
    case FileInput(file) => TNothing // do something with the file
  }
}

object Toml extends Toml