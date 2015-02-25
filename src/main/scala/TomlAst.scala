/**
 * @author Matt Torres
 */
object TomlAst {
  sealed abstract class TValue {
    type Values

    def values: Values

    def children: List[TValue] = Nil

    def apply(i: Int): TValue = TNothing

    case object TNothing extends TValue {
      type Values = None.type
      def values = None
    }

    case class TString(s: String) extends TValue {
      type Values = String
      def values = s
    }


  }
}
