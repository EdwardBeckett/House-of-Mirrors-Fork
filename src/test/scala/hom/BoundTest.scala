package hom

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

class BoundTest extends AssertionsForJUnit {

  @Test
  def easyContainment() {
    val sut = Bound(3, 3, 7, 7)
    assert(sut.contains(Point(5,5)))
    assert(sut.contains(Point(3,3)))
    assert(sut.contains(Point(9,9)))
    assertFalse(sut.contains(Point(10,10)))
    assertFalse(sut.contains(Point(2,1)))
    assertFalse(sut.contains(Point(2,6)))
    assertFalse(sut.contains(Point(6,2)))
    assertFalse(sut.contains(Point(10,5)))
    assertFalse(sut.contains(Point(5,10)))
  }

  @Test
  def pathologicalContainment() {
    assert(Bound(3, 3, Int.MaxValue, 10).contains(Point(5,5)))
    assert(Bound(3, 3, 10, Int.MaxValue).contains(Point(5,5)))
  }

  @Test
  def negativeWidthContainsNothing() {
    assertFalse(Bound(3, 3, -100, 10).contains(Point(0,5)))
  }

  @Test
  def easyContainmentOneOff() {
    val sut = Bound(3, 3, 7, 7)
    assert(sut.containsOneOff(Point(5,5)))
    assert(sut.containsOneOff(Point(3,3)))
    assert(sut.containsOneOff(Point(9,9)))
    assert(sut.containsOneOff(Point(10,10)))
    assertFalse(sut.containsOneOff(Point(11,11)))
    assertFalse(sut.containsOneOff(Point(2,1)))
    assert(sut.containsOneOff(Point(2,6)))
    assert(sut.containsOneOff(Point(6,2)))
    assert(sut.containsOneOff(Point(10,5)))
    assert(sut.containsOneOff(Point(5,10)))
    assertFalse(sut.containsOneOff(Point(5,11)))
  }

  @Test
  def caseEquality() {
    assert(Bound(1,2,3,4) === Bound(1,2,3,4))
  }
}

