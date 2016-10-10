package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- Gen.frequency((1, empty), (2, genHeap))
  } yield insert(v, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll {
    (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
  }

  property("minimum") = forAll {
    a: Int =>
      val heap = insert(a, empty)
      findMin(heap) == a
  }

  property("emptyOrDelete") = forAll {
    a: Int =>
      val heap = insert(a, empty)
      isEmpty(deleteMin(heap))
  }

  property("minimumOfTwo") = forAll {
    (a: Int, b: Int) =>
      val heap = insert(b, insert(a, empty))
      findMin(heap) == List(a, b).min
  }

  property("meld") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == List(findMin(a), findMin(b)).min
  }

  property("sorted") = forAll { (h: H) =>
    def delete(a: H, acc: List[Int]): List[Int] = {
      if (isEmpty(a)) acc
      else {
        val min = findMin(a)
        delete(deleteMin(a), acc :+ min)
      }
    }
    var result = delete(h, List())
    result == result.sorted
  }

  property("delete") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(h)) == List(a, b, c).sorted.drop(1).head
  }

}
