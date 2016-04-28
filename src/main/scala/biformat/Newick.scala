package biformat

import java.io.{File, FileReader}

import scala.util.parsing.combinator.JavaTokenParsers

object Newick extends NewickParser{

  def fromFile(f: File): Root = {
    val reader = new FileReader(f)
    val result = parseAll(root, reader).get
    reader.close()
    result
  }

  trait Tree {
    def toList: List[Double]
  }

  trait Parent extends Tree{
    def children: List[Child]
    def chList: List[Double] = children.foldLeft(Nil:List[Double]){(n, x) => x.toList ::: n}
  }

  trait Child extends Tree{
    def branch: Double
  }

  case class Root(children: List[Child]) extends Parent{
    def toList = chList.reverse
  }

  case class Trunk(branch: Double, children: List[Child]) extends Child with Parent{
    def toList = branch :: chList
  }

  case class Leaf(branch: Double, name: String) extends Child{
    def toList = branch :: Nil
  }

}

trait NewickParser extends JavaTokenParsers {
  import Newick._

  def nodeList: Parser[List[Child]] = "(" ~> repsep(node, ",") <~ ")" ^^ {_}

  def root: Parser[Root] = nodeList <~ ";" ^^ {Root(_)}

  def node: Parser[Child] = nodeList ~ ":" ~ branch ^^ { case nl ~ ":" ~ value => Trunk(value.toDouble, nl) } | leaf

  def leaf: Parser[Leaf] = name ~ ":" ~ branch ^^ { case name ~ ":" ~ value => Leaf(value.toDouble, name) }

  def branch: Parser[Double] = floatingPointNumber ^^ {_.toDouble}

  def name: Parser[String] = "[a-zA-Z0-9_.'-]+".r
}