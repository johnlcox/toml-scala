package com.leacox.toml

/**
 * @author John Leacox
 */
object TomlAst {

  object TValue

  sealed abstract class TValue {
    type Values

    def values: Values

    def children: List[TValue] = this match {
      case TObject(value) => value map (_._2)
      case TArray(value) => value
      case _ => Nil
    }

    def apply(i: Int): TValue = TNothing

    def ++(other: TValue) = {
      def append(value1: TValue, value2: TValue): TValue = (value1, value2) match {
        case (TNothing, x) => x
        case (x, TNothing) => x
        case (TArray(xs), TArray(ys)) => TArray(xs ::: ys)
        case (TArray(xs), x: TValue) => TArray(xs ::: List(x))
        case (x: TValue, TArray(xs)) => TArray(x :: xs)
        case (x, y) => TArray(x :: y :: Nil)
      }
      append(this, other)
    }

    def toOption: Option[TValue] = this match {
      case TNothing => None
      case toml => Some(toml)
    }
  }

  case object TNothing extends TValue {
    type Values = None.type
    def values = None
  }

  case class TString(s: String) extends TValue {
    type Values = String
    def values = s
  }

  case class TDouble(d: Double) extends TValue {
    type Values = Double
    def values = d
  }

  case class TInteger(l: Long) extends TValue {
    type Values = Long
    def values = l
  }

  case class TBoolean(b: Boolean) extends TValue {
    type Values = Boolean
    def values = b
  }

  case class TObject(obj: List[TField]) extends TValue {
    type Values = Map[String, Any]
    def values = obj.map { case (n, v) => (n, v.values)} toMap
  }

  case object TObject {
    def apply(fs: TField*): TObject = TObject(fs.toList)
  }

  case class TArray(arr: List[TValue]) extends TValue {
    type Values = List[Any]
    def values = arr.map(_.values)
    override def apply(i: Int): TValue = arr(i)
  }

  type TField = (String, TValue)

  object TField {
    def apply(name: String, value: TValue) = (name, value)
    def unapply(f: TField): Option[(String, TValue)] = Some(f)
  }
}
