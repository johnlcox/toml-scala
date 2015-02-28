package com.leacox

import java.io.{File, InputStream, Reader => JReader}

/**
 * @author John Leacox
 */
package object toml {
  type TValue = TomlAst.TValue
  val TNothing = TomlAst.TNothing
  type TString = TomlAst.TString
  val TString = TomlAst.TString
  type TDouble = TomlAst.TDouble
  val TDouble = TomlAst.TDouble
  type TInteger = TomlAst.TInteger
  val TInteger = TomlAst.TInteger
  type TBoolean = TomlAst.TBoolean
  val TBoolean = TomlAst.TBoolean
  type TObject = TomlAst.TObject
  val TObject = TomlAst.TObject
  type TArray = TomlAst.TArray
  val TArray = TomlAst.TArray
  type TField = TomlAst.TField
  val TField = TomlAst.TField

  implicit def string2TomlInput(s: String): TomlInput = StringInput(s)
  implicit def reader2TomlInput(reader: JReader): TomlInput = ReaderInput(reader)
  implicit def stream2TomlInput(stream: InputStream): TomlInput = StreamInput(stream)
  implicit def file2TomlInput(file: File): TomlInput = FileInput(file)
}
