
package hom

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.Seq

class GateTest extends AssertionsForJUnit {

  @Test(expected=classOf[UnsupportedOperationException])
  def fixedGateRefusesToMove() {
    val sut = new FixedWormHole(Point(1,1), Point(5,5))
    sut.position = Point(13,13)
  }
}
