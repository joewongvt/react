package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: A, b: A) =>
    val heapWithA = insert(a, empty)
    val heapWithAandB = insert(b, heapWithA)
    val min = findMin(heapWithAandB)
    if (a < b) {
      min == a
    } else {
      min == b
    }
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insert element then delete min should produce empty") = forAll{ a:A =>
    val addOne = insert(a, empty)
    val removeIt = deleteMin(addOne)
    isEmpty(removeIt)
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("continuallyFindAndDeleteMinima") = forAll{ h:H =>
    val firstMin = findMin(h)
    compareMin(firstMin, deleteMin(h))
  }

  def compareMin(prevMin:A, currentHeap:H): Boolean = {
    if (isEmpty(currentHeap)) {
      true
    } else {
      val newMin = findMin(currentHeap)
      if (newMin >= prevMin) {
        compareMin(newMin, deleteMin(currentHeap))
      } else {
        false
      }
    }
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min of meld") = forAll{ (h1:H, h2:H) =>
    val a = findMin(h1)
    val b = findMin(h2)
    val c = findMin(meld(h1, h2))
    (c == a) || (c == b)
  }

  // compare two heaps by successively comparing and deleting min
  def compareHeaps(h1:H, h2:H):Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) {
      true
    } else if (isEmpty(h1) || isEmpty(h2)) {
      false
    } else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      if (m1 == m2) {
        compareHeaps(deleteMin(h1), deleteMin(h2))
      } else {
        false
      }
    }
  }

  property("heap is equal to itself") = forAll{ (h:H) =>
    compareHeaps(h,h)
  }

  def convertListToHeap(h:H, l:List[A]):H = {
    if (l.isEmpty) {
      h
    } else {
      convertListToHeap(insert(l.head, h), l.tail)
    }
  }

  property("Convert single element list into heap always returns single element as min") = forAll{ a:A =>
    val h:H = convertListToHeap(empty, List(a))
    findMin(h) == a
  }

  property("insertion order into the heap does not matter") = forAll{ (l:List[A]) =>
    val h1 = convertListToHeap(empty, l)
    val h2 = convertListToHeap(empty, l.reverse)
    compareHeaps(h1, h2)
  }


  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
