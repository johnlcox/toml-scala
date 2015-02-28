package toml

import org.joda.time.DateTime

/**
 * @author John Leacox
 */
sealed abstract class TValue {

  def \(fieldName: String): TValue = TUndefined(s"'$fieldName' is undefined on object: $this")

  def apply(index: Int): TValue = TUndefined(s"${this.toString} is not an array")

  def \\(fieldName: String): Seq[TValue] = Nil

  //  def asOpt[T](implicit reads: Reads[T]): Option[T] = reads.reads(this).fold(
  //    invalid = _ => None,
  //    valid = v = Some(v)
  //  ).filter {
  //    case TUndefined() => false
  //    case _ => true
  //  }

  //  def as[T](implicit reads: Reads[T]): T = reads.reads(this).fold(
  //    valid = identity,
  //    invalid = e => throw new TResultException(e)
  //  )

  override def toString: String

  class TUndefined(err: => String) extends TValue {
    def error = err
    override def toString = "TUndefined(" + err + ")"
  }

  object TUndefined {
    def apply(err: => String) = new TUndefined(err)
    def unapply(o: Object): Boolean = o.isInstanceOf[TUndefined]
  }

  case class TBoolean(value: Boolean) extends TValue {
    override def toString = String.valueOf(value)
  }

  case class TDouble(value: Double) extends TValue {
    override def toString = String.valueOf(value)
  }

  case class TInteger(value: Long) extends TValue {
    override def toString = String.valueOf(value)
  }

  case class TString(value: String) extends TValue {
    override def toString = value
  }

  case class TDateTime(value: DateTime) extends TValue {
    override def toString = null // TODO
  }

  case class TArray(value: Seq[TValue] = List()) extends TValue {
    override def apply(index: Int): TValue = {
      value.lift(index).getOrElse(TUndefined("Array index out of bounds in " + this))
    }

    override def \\(fieldName: String): Seq[TValue] = value.flatMap(_ \\ fieldName)

    def ++(other: TArray): TArray = TArray(value ++ other.value)

    def :+(el: TValue): TArray = TArray(value :+ el)
    def append(el: TValue): TArray = this.:+(el)

    def +:(el: TValue): TArray = TArray(el +: value)
    def prepend(el: TValue): TArray = this.+:(el)
  }

  case class TObject(fields: Seq[(String, TValue)]) extends TValue {
  }

}
