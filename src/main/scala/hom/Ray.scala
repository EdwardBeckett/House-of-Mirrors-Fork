package hom

/**
 * A Bound is just like a Rectangle.
 */
case class Bound(x: Int, y: Int, width: Int, height: Int) {
  def this(topL: Point, botR: Point) = this(topL.x, topL.y, botR.x - topL.x, botR.y - topL.y)

  //require(width > 0)
  //require(height > 0)

  /** The same as Rectangle.contains */
  def contains(p: Point): Boolean = contains(p.x, p.y, 0)

  /** The same as contains on a rectangle that is larger by one unit in each direction */
  def containsOneOff(p: Point) = contains(p.x, p.y, 1)

  private def contains(ex: Int, why: Int, offset: Int): Boolean = {
    val loX = this.x - offset
    val loY = this.y - offset
    if ((this.width | this.height) < 0) {
      false
    } else if (ex < loX || why < loY) {
      false
    } else {
      val hiX = this.x + this.width + offset
      val hiY = this.y + this.height + offset
      (hiX < loX || hiX > ex) && (hiY < loY || hiY > why)
    }
  }
}

import Directions._
import LineColors._

case class Segment(val start: Point, val end: Point, val color: LineColor) {
  override def toString = "Segment:" + start + " -> " + end
}

case class Ray(val start: Point, val direction: Direction, val color: LineColor) {
  def nextPoint(p: Point) = this.direction match {
    case North => Point(p.x, p.y - 1)
    case Northeast => Point(p.x + 1, p.y - 1)
    case East => Point(p.x + 1, p.y)
    case Southeast => Point(p.x + 1, p.y + 1)
    case South => Point(p.x, p.y + 1)
    case Southwest => Point(p.x - 1, p.y + 1)
    case West => Point(p.x - 1, p.y)
    case Northwest => Point(p.x - 1, p.y - 1)
  }
  override def toString = "Ray " + this.start + " in " + this.direction
}


