import biformat.{BedIterator, WigIterator}
import org.scalatest.FunSuite

import scala.io.Source

class WigTest extends FunSuite {
  test("merger"){
    val fi = "src/test/resources/biformat/sample.var.wig"
    val its = WigIterator.fromFile(fi).merge().map(_.marginalize(2)).iterator
    val x1 = its.next()
    assert(x1.lines sameElements  Array((95,22.0),(96,23.0)))
  }
  test("honban"){
    val fi = "src/test/resources/chr21.head10000.bls.wig"
    val its = WigIterator.fromFile(fi).merge().map(_.marginalize(2)).iterator
    val x1 = its.next()
  }
  test("bedfilter"){
    val fwig = "src/test/resources/biformat/sample.var.wig"
    val fbed = "src/test/resources/biformat/sample.bed"
    val its = WigIterator.fromFile(fwig)
    val bits = BedIterator.fromSource(Source.fromFile(fbed))
    val tmp = its.filterWithBed(bits).iterator
    assert(tmp.next.lines sameElements Array((93,5.0),(94,6.0)))
    assert(tmp.next.lines sameElements Array((97,3.0),(98,6.0)))
    assert(tmp.next.lines sameElements Array((117,8.0)))
    assert(!tmp.hasNext)
  }
}
