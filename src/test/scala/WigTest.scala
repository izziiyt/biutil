import biformat.{BedIterator, WigIterator, bigSource}
import org.scalatest.FunSuite

class WigTest extends FunSuite {
  test("merger"){
    val fi = bigSource("src/test/resources/biformat/sample.var.wig")
    val its = WigIterator.fromSource(fi).merge().map(_.marginalize(2)).iterator
    val x1 = its.next()
    assert(x1.lines sameElements  Array((95,22.0),(96,23.0)))
  }
  test("honban"){
    val fi = bigSource("src/test/resources/chr21.head10000.bls.wig")
    val its = WigIterator.fromSource(fi).merge().map(_.marginalize(2)).iterator
    val x1 = its.next()
  }
  test("bedfilter"){
    val fwig = bigSource("src/test/resources/biformat/sample.var.wig")
    val fbed = bigSource("src/test/resources/biformat/sample.bed")
    val its = WigIterator.fromSource(fwig)
    val bits = BedIterator.fromSource(fbed).filter(x => x.chr == "chr21")
    val tmp = its.filterWithBed(bits).iterator
    assert(tmp.next.lines sameElements Array((93,5.0),(94,6.0)))
    assert(tmp.next.lines sameElements Array((97,3.0),(98,6.0)))
    assert(tmp.next.lines sameElements Array((117,8.0)))
    assert(!tmp.hasNext)
  }

  test("hist"){
    val fwig = bigSource("src/test/resources/biformat/sample.var.wig")
    val its = WigIterator.fromSource(fwig)
    val tmp = its.hist(5,10)
    assert(tmp sameElements Array(2,2,1,3,1))
  }
}
