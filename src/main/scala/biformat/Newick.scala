package biformat

import java.io.{File, FileReader}

import scala.util.parsing.combinator._

object Newick {

  trait Tree {
    private[Newick] def toList: List[Tree]

  }

  trait Parent extends Tree{
    private def mappedChildList[T](f: Child => List[T]): List[T] = children.foldLeft(Nil: List[T]) { (n, x) => f(x) ::: n }
    val children: List[Child]
    private[Newick] def chList: List[Child] = mappedChildList(_.toList)
    private[Newick] def chDFMLL(cl: List[Child], d: Double): List[Child] =
      cl.foldRight(Nil:List[Child]){(x, xs) => x.toDFMLL(d)._2 :: xs}
  }

  trait Child extends Tree{
    val branch: Double
    private[Newick] def toDFMLL(d:Double): (Double, Child)
    def toList: List[Child]
  }

  case class Root(children: List[Child]) extends Parent{
    /**
      * @return List[Tree] which is depth-first searced order
      */
    def toList = chList.reverse
    /**
      * @return Tree in which branch value is "Distance From the Most Left Leaf"
      */
    def toDFMLL: Root = {
      val (ld, ll) = children.head.toDFMLL(0)
      Root(ll :: chDFMLL(children.tail, ld))
    }
  }

  case class Trunk(branch: Double, children: List[Child]) extends Child with Parent {
    def toList = this :: chList

    def toDFMLL(d: Double): (Double, Trunk) =
      if (d == 0) {
        val (ld, ll) = children.head.toDFMLL(0)
        (ld + branch, Trunk(ld, ll :: chDFMLL(children.tail,ld)))
      }
      else {
        (Double.MaxValue, Trunk(d + branch, chDFMLL(children, d + branch)))
      }
  }

  case class Leaf(branch: Double, name: String) extends Child{
    def toList = this :: Nil
    def toDFMLL(d: Double):(Double, Leaf) =
      if(d == 0) (branch, Leaf(0, name))
      else (Double.MaxValue, Leaf(d + branch, name))
  }

}

object NewickParsers extends JavaTokenParsers {
  import Newick._
  def fromFile(f: File): Root = {
    val reader = new FileReader(f)
    val result = parseAll(root, reader).get
    reader.close()
    result
  }

  private def children: Parser[List[Child]] = "(" ~> repsep(child, ",") <~ ")"

  private def root: Parser[Root] = children <~ ";" ^^ Root

  private def child: Parser[Child] = children ~ ":" ~ floatingPointNumber ^^
    { case nl ~ ":" ~ value => Trunk(value.toDouble, nl) } | leaf

  private def leaf: Parser[Leaf] = name ~ ":" ~ floatingPointNumber ^^
    { case name ~ ":" ~ value => Leaf(value.toDouble, name) }

  private def name: Parser[String] = "[a-zA-Z0-9_.'-]+".r
}
