package hom

import Directions._

case class Point(x: Int, y: Int) {
  def moveUp = Point(x, y - 1)
  def moveDown = Point(x, y + 1)
  def moveLeft = Point(x - 1, y)
  def moveRight = Point(x + 1, y)

  /* Gives the next scaled point in this direction */
  def nextPoint(d: Direction, hscale: Int, vscale: Int): Point = {
    def htilt = (0.7071067811865475 * hscale).toInt
    def vtilt = (0.7071067811865475 * vscale).toInt
    d match {
      case North => Point(this.x, this.y - vscale)
      case Northeast => Point(this.x + htilt, this.y - vtilt)
      case East => Point(this.x + hscale, this.y)
      case Southeast => Point(this.x + htilt, this.y + vtilt)
      case South => Point(this.x, this.y + vscale)
      case Southwest => Point(this.x - htilt, this.y + vtilt)
      case West => Point(this.x - hscale, this.y)
      case Northwest => Point(this.x - htilt, this.y - vtilt)
    }
  }
}
