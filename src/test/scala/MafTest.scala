import biformat.Maf.MafIterator
import org.scalatest.FunSuite
import alignment.Base.{A,C,G,T,N}
import alignment.Base
class MafTest extends FunSuite {
  /*test("readmaf") {
      def f(xs:List[Array[Base]],ys:List[Array[Base]]):Unit = assert((xs,ys).zipped.forall((x,y) => x sameElements y))
      val xs1 = Maf.readMaf("src/test/resources/fdur/test1.maf", 10)
      val ys1 = List(Array[Base](C, A, G, C, A, C, T, T), Array[Base](C, A, G, G, A, G, T, T), Array[Base](C, T, G, G, T, C, G, G))
      f(xs1.head,ys1)
      val xs2 = Maf.readMaf("src/test/resources/fdur/sample.maf", 10)
      val ys2 = Array(
        List(Array[Base](C, A, G, C, A, C, T, T,C,A), Array[Base](C, A, G, G, A, G, T, T,C,A), Array[Base](C, T, G, G, T, C, G, G,C,T)),
        List(Array[Base](G,C,A,C,T,T,A,A,T), Array[Base](G,G,A,G,T,T,N,N,T), Array[Base](G,G,T,C,G,G,N,N,G))
      )
      (xs2,ys2).zipped.foreach((as,bs) => f(as,bs))
    }*/

  test("MafIterator"){
    val its = MafIterator.fromMSA("src/test/resources/biformat/sample.maf").iterator
    val x1 = its.next()
    val x2 = its.next()
    val x3 = its.next()
    assert(!its.hasNext)
    val tmp = x1 + x2 + x3
    //tmp.lines.values.foreach{y => println(y.name + " " + y.seq.mkString(",") + " " + y.length)}
    assert(tmp.lines("beta").seq sameElements Array(C,A,G,G,A,G,T,T,N,N,N,N,N,N,N,N,N,N,N,A,G,N))
    assert(tmp.lines("alpha").seq sameElements Array(C,A,G,C,A,C,T,T,C,A,G,C,A,C,T,T,A,A,T,C,A,N))
    assert(tmp.lines("gamma").seq sameElements Array(C,T,G,G,T,C,G,G,C,T,G,G,T,C,G,G,N,N,G,N,N,N))
    assert(tmp.lines("beta").length == 22 && tmp.lines("alpha").length == 22 && tmp.lines("gamma").length == 22)

    //its.foreach(x => x.lines.values.foreach(y => println(y.seq.mkString(","))))
  }
}
