package com.leacox.toml

import java.io.{File, InputStream, Reader => JReader}

/**
 * @author John Leacox
 */
sealed trait TomlInput
case class StringInput(string: String) extends TomlInput
case class ReaderInput(reader: JReader) extends TomlInput
case class StreamInput(stream: InputStream) extends TomlInput
case class FileInput(file: File) extends TomlInput
