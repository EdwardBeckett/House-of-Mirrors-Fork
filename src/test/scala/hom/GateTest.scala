
package hom

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.Seq

import LineColors._
import Directions._

class GateTest extends AssertionsForJUnit {

  @Test
  def blockerIsOpaque() {
    val sut = new FixedBlocker(Point(1,1))
    val ray = Ray(Point(2,3), North, Red)
    val (rays, g) = sut.act(ray)
    assertTrue(rays.isEmpty)
    assertTrue(g eq sut)
  }

  @Test
  def moveableBlockerCanMove() {
    val sut = new MoveableBlocker(Point(1,1))
    val result = sut.moveTo(Point(2,2))
    assertTrue(result ne sut)
    val back = result.asInstanceOf[Moveable].moveTo(Point(1,1))
    assertEquals(sut, back)
    val ray = Ray(Point(2,3), North, Red)
    val (rays, g) = sut.act(ray)
    assertTrue(rays.isEmpty)
    assertTrue(g eq sut)
  }

  @Test
  def moveableSourceCanMoveAndRotate() {
    val sut = new MoveableSource(Point(1,1), North, Red)
    val result = sut.moveTo(Point(2,2))
    assertTrue(result ne sut)
    assertEquals(Point(2,2), result.position)
    val res2 = sut.turnCW
    assertEquals(Northeast, res2.asInstanceOf[Oriented].direction)
  }
}
