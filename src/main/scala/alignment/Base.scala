package alignment

trait Base{
  def toChar: Char
  def toInt: Int = Base.toInt(this)
  def nonNuc: Boolean = false
}

object Base{

  val binLength:Int = 3

  case object A extends Base {def toChar = 'A'}
  case object C extends Base {def toChar = 'C'}
  case object G extends Base {def toChar = 'G'}
  case object T extends Base {def toChar = 'T'}

  /**
    * Unknown signal
    */
  case object N extends Base {
    def toChar = 'N'
    override def nonNuc = true
    //override def equals(that: Any) = false
  }

  /**
    * Deletion
    */
  case object D extends Base {
    def toChar = '-'
    override def toString = "-"
    override def nonNuc = true
  }

  val fromInt: Int => Base =
    Array(A, C, G, T, N, D)

  val toInt: Base => Int =
    Map(A -> 0, C -> 1, G -> 2, T -> 3, N -> 4, D -> 5)

  val fromChar: Char => Base = {
    case 'a' | 'A' => A
    case 't' | 'T' => T
    case 'c' | 'C' => C
    case 'g' | 'G' => G
    case '-' => D
    case ' ' => sys.error("Base.fromChar doesn't accept white space.")
    case _ => N
  }
}

