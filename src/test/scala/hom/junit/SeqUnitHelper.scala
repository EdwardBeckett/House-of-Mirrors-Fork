
package hom.junit

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.Seq

trait SeqUnitHelper {

  /**
   * Passes if two sequences correspond equally.
   */
  def coequals[A,B](a: Seq[A])(b: Seq[B]) {
    corresponds(a, b) {
      (x,y) => x == y
    }
  }

  /**
   * Passes if two sequences correspond with respect to
   * the given predicate, i.e. a.corresponds(b)(p) == true.
   */
  def corresponds[A,B](a: Seq[A], b: Seq[B])(p: (A,B) => Boolean) {
    val ia = a.iterator
    val ib = b.iterator

    var i = 0;
    while (ia.hasNext && ib.hasNext) {
      if (!p(ia.next, ib.next)) failAt(i, a, b)
      i += 1
    }
    if (ia.hasNext || ib.hasNext) failAt(i, a, b)
  }

  private def failAt[A,B](i: Int, a: Seq[A], b: Seq[B]) {
    throw new CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
  }

}
