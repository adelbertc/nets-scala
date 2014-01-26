package nets

import scala.annotation.tailrec
import scala.collection.immutable.Seq

import scalaz.{ Equal, Monoid, Order }
import scalaz.syntax.order._

/** Set that supports O(logn) insert for O(1) random access */
final class IndexedSet[A] private(private val repr: Vector[A]) {
  def contains(x: A)(implicit A: Order[A]): Boolean =
    member(x)

  def delete(x: A)(implicit A: Order[A]): IndexedSet[A] = {
    val (membership, n) = IndexedSet.binarySearch(repr, x)
    if (membership) new IndexedSet(repr.take(n) ++ repr.drop(n + 1))
    else this
  }

  def difference(other: IndexedSet[A])(implicit A: Order[A]): IndexedSet[A] = {
    val tempSet =
      this.foldLeft(IndexedSet.empty[A]) { (set, x) =>
        if (other.notMember(x)) set.insert(x)
        else set
      }

    other.foldLeft(tempSet) { (set, x) =>
      if (this.notMember(x)) set.insert(x)
      else set
    }
  }

  def elementAt(i: Int): Option[A] =
    try { Option(repr(i)) }
    catch { case _: Throwable => None }

  def filter(p: A => Boolean): IndexedSet[A] =
    new IndexedSet(repr.filter(p))

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    repr.foldLeft(z)(f)

  def foldRight[B](z: B)(f: (A, B) => B): B =
    repr.foldRight(z)(f)

  def indexOf(x: A)(implicit A: Order[A]): Option[Int] = {
    val (elemIsThere, index) = IndexedSet.binarySearch(repr, x)
    if (elemIsThere) Some(index) else None
  }

  def insert(x: A)(implicit A: Order[A]): IndexedSet[A] = {
    val (membership, index) = IndexedSet.binarySearch(repr, x)
    if (membership) this
    else new IndexedSet(IndexedSet.insertAt(repr, x, index))
  }

  def intersection(other: IndexedSet[A])(implicit A: Order[A]): IndexedSet[A] = {
    this.foldLeft(IndexedSet.empty[A]) { (set, x) =>
      if (other.member(x)) set.insert(x)
      else set
    }
  }

  def isProperSubsetOf(other: IndexedSet[A])(implicit A: Equal[A]): Boolean =
    (this.size < other.size) && isSubsetOf(other)

  def isSubsetOf(other: IndexedSet[A])(implicit A: Equal[A]): Boolean = {
    @tailrec
    def isSubsetOfAux(xs: Vector[A], ys: Vector[A]): Boolean =
      if (xs.isEmpty) true
      else if (ys.isEmpty) false
      else if (xs.head === ys.head) isSubsetOfAux(xs.tail, ys.tail)
      else false

    (this.size <= other.size) && isSubsetOfAux(repr, other.repr)
  }

  def maxOption: Option[A] = repr.lastOption

  def member(x: A)(implicit A: Order[A]): Boolean =
    indexOf(x).isDefined

  def minOption: Option[A] = repr.headOption

  def notMember(x: A)(implicit A: Order[A]): Boolean =
    !member(x)

  def partition(p: A => Boolean): (IndexedSet[A], IndexedSet[A]) = {
    val (l, r) = repr.partition(p)
    (new IndexedSet(l), new IndexedSet(r))
  }

  val size: Int = repr.size

  def toList: List[A] = repr.toList

  val toVector: Vector[A] = repr

  def union(other: IndexedSet[A])(implicit A: Order[A]): IndexedSet[A] =
    other.foldLeft(this) { (set, x) =>
      set.insert(x)
    }
}

object IndexedSet extends IndexedSetInstances {
  def empty[A : Order]: IndexedSet[A] =
    new IndexedSet(Vector.empty)

  def fromSeq[A : Order](xs: Seq[A]): IndexedSet[A] =
    xs.foldLeft(IndexedSet.empty[A])((xs, x) => xs.insert(x))

  def singleton[A : Order](x: A): IndexedSet[A] =
    new IndexedSet(Vector(x))

  private def insertAt[A](xs: Vector[A], x: A, n: Int): Vector[A] =
    xs.take(n) ++ Vector(x) ++ xs.drop(n)

  private def binarySearch[A](repr: Vector[A], elem: A)(implicit A: Order[A]): (Boolean, Int) = {
    @tailrec
    def binarySearchAux(minIndex: Int, maxIndex: Int): (Boolean, Int) = {
      val midIndex = minIndex + ((maxIndex - minIndex) / 2)

      if (maxIndex < minIndex) (false, midIndex)
      else {
        if (repr(midIndex) > elem) binarySearchAux(minIndex, midIndex - 1)
        else if (repr(midIndex) < elem) binarySearchAux(midIndex + 1, maxIndex)
        else (true, midIndex)
      }
    }
    binarySearchAux(0, repr.size - 1)
  }
}

trait IndexedSetInstances {
  implicit def indexedSetMonoid[A : Order]: Monoid[IndexedSet[A]] =
    new Monoid[IndexedSet[A]] {
      override def append(f1: IndexedSet[A], f2: => IndexedSet[A]): IndexedSet[A] = f1.union(f2)

      override def zero: IndexedSet[A] = IndexedSet.empty
    }
}
