package alignment

import org.scalatest.FunSuite

class DNATest extends FunSuite {
  import Base._
  test("foreach & map"){
    val x = Array(A,C,G,T,N)
    val z = DNA.fromSeq(x)
    z.foreach(println)
    println(z.map(y => 1))
  }

}
