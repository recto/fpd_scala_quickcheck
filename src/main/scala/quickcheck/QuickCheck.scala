package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Insert two elements and find the min properly.") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("Insert an element and deleting the minimal results in an empty Heap.") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("Get the sorted sequence by recursively finding/deleting the min.") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  property("The minimal value of a melded Heap should be the min of one or the other.") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("Melded heaps should be same.") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
