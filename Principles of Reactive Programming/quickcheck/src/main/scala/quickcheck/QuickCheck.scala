package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1: If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") =
    forAll { (a: Int, b: Int) =>
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      findMin(h2) == math.min(a, b)
    }

  property("hint2: If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (a: Int) =>
      val h1 = insert(a, empty)
      val h2 = deleteMin(h1)
      isEmpty(h2) == true
    }

  property("hint3: Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") =
    forAll { (h: H) =>
      def getElems(heap: H): List[Int] = {
        isEmpty(heap) match {
          case true => {
            Nil
          }
          case false => {
            val m = findMin(heap)
            val remaining = deleteMin(heap)
            m :: getElems(remaining)
          }
        }
      }
      val elems = getElems(h)
      elems.sorted == elems
    }

  property("hint4: Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (h1: H, h2: H) =>
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val h = meld(h1, h2)
      val m = findMin(h)
      (m == m1) || (m == m2)
    }

  property("Finding a minimum of the melding of a heap with an empty heap should return the minimum of the heap.") =
    forAll { (h: H) =>
      val m = findMin(h)
      val h1 = meld(h, empty)
      val h2 = meld(empty, h)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      (m == m1) && (m == m2) && (m1 == m2)
    }

  property("Finding a minimum of the melding of any two identical heaps should return a minimum of one or the other.") =
    forAll { (h: H) =>
      val m = findMin(h)
      val h1 = meld(h, h)
      val m1 = findMin(h1)
      (m == m1)
    }

  property("Heap sorts") =
    forAll(Gen.choose(0, 100)) { (n: Int) =>
      def getElems(heap: H): List[Int] = {
        isEmpty(heap) match {
          case true => {
            Nil
          }
          case false => {
            val m = findMin(heap)
            val remaining = deleteMin(heap)
            m :: getElems(remaining)
          }
        }
      }
      val toInsert = 1 to n
      var h = empty
      for (num <- toInsert) {
        h = insert(num, h)
      }
      val elems = getElems(h)
      elems == toInsert
    }
}
