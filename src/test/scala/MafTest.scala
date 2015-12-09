import biformat.MafIterator
import org.scalatest.FunSuite

class MafTest extends FunSuite {

  test("construct"){
    val s = biformat.bigSource("src/test/resources/biformat/test.maf")
    val its = MafIterator.fromMSA(s,"hg19").iterator
    val x1 = its.next()
    assert(x1.target == "hg19")
    assert(x1.start == 16091775)
    assert(x1.lines.size == 2)
    assert(x1.end == 16091781)
    its.next()
    val x2 = its.next()
    assert(x2.target == "hg19")
    assert(x2.start == 16091789)
    assert(x2.lines.size == 3)
    assert(x2.end == 16091801)
    its.next()
    assert(!its.hasNext)
  }

  test("merge"){
    val s = biformat.bigSource("src/test/resources/biformat/test.maf")
    val its = MafIterator.fromMSA(s,"hg19").merge(20).iterator
    val x1 = its.next()
    assert(x1.start == 16091775)
    assert(x1.end == 16091788)
    assert(x1.lines("panTro4").seq.mkString("") == "---------TG--CC-T")
    val x2 = x1.Dremoved
    assert(x2.start == 16091775)
    assert(x2.end == 16091788)
    assert(x2.lines("gorGor3").seq.mkString("") == "T-ACATCGGTCCT")
    val x3 = its.next().Dremoved
    assert(x3.start == 16091789)
    assert(x3.end == 16091801)
    assert(x3.lines("gorGor3").seq.mkString("") == "AACTAGACCCT")
    its.next()
    assert(!its.hasNext)
  }
}
