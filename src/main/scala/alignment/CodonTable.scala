package alignment

import java.io.{File, FileReader}
import alignment.Base._
import scala.collection._
import scala.util.parsing.combinator.JavaTokenParsers

case class CodonTable(is4d:Map[Codon,Boolean],cdn2aa:Map[Codon,AminoAcid]){
  def transcript(cdn: Codon): AminoAcid = cdn2aa.get(cdn) match {
    case Some(x) => x
    case None => AminoAcid.X
  }
  def is4Fold(cdn: Codon): Boolean = is4d.get(cdn) match {
    case Some(x) => x
    case None => false
  }
}

object CodonTable extends CodonTableParser{

  protected val bases = Array(A,C,G,T)

  def fromFile(inf: File): CodonTable = {
    val c2a = parseAll(blocks,new FileReader(inf)).get
    val is4Fold = mutable.Map[Codon,Boolean]()
    for(b1 <- bases;b2 <- bases;cdns = addOnes(b1,b2)){
      val aas:Array[AminoAcid] = cdns.map(c2a(_))
      val f = if(aas.forall(_ == aas(0))) true else false
      cdns.foreach(is4Fold += _ -> f)
    }
    new CodonTable(is4Fold,c2a)
  }

  def fromFile(inf: String): CodonTable = fromFile(new File(inf))

  protected def addOnes(f:Base,s:Base):Array[Codon] = (bases :+ N).map(Codon(f,s,_))
}

class CodonTableParser extends JavaTokenParsers {

  def block:Parser[Block] = cdn~aa<~""".{3}""".r ^^ {case x~y => Block(x,y)}

  def cdn:Parser[Codon] = """[ATUCGN]{3}""".r ^^ {Codon(_)}

  def aa:Parser[AminoAcid] = """.""".r ^^ {case x => AminoAcid.fromChar(x.head)}

  def blocks:Parser[Map[Codon,AminoAcid]] = rep(block) ^^ {_.map{case Block(x,y) => x -> y}.toMap}

  case class Block(cdn:Codon,amnacd:AminoAcid)
}