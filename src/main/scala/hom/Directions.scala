package hom

/**
 * Points of the compass.
 */
object Directions extends Enumeration {
  
  val North, Northeast, East, Southeast, South, Southwest, West, Northwest = Direction
  
  // class param initial to Enumeration
  private val minId = North.id
  
  // factory function is a workaround, since no-arg ctor to Val breaks introspected naming in 2.8.0
  private def Direction = new Direction(nextId, null)

  /** An element of Directions, with utility methods for simple transforms. */
  class Direction protected[Directions] (i: Int, s: String) extends Val(i, s) {
    def right45 = lookup(bump)
    def left45 = lookup(decr)
    def right90 = right45.right45
    def left90 = left45.left45
    def right135 = right45.right90
    def left135 = left45.left90
    //def reverse = right90.right90
    // a bit of premature optimization, as an exercise
    def reverse = lookup(bump(this.id, 4))

    def angle = (this.id - minId) * 45
    def angle(other: Direction): Int = mod(angle - other.angle, 360)
    /** Angular difference in degrees, denoted by minus and angle bracket. */
    def -<(that: Direction) = angle(that)

    /** Test for parallel. */
    def ?||(that: Direction) = isParallelTo(that)
    def isParallelTo(that: Direction) = (this == that) || (reverse == that)
    
    // Enumeration.apply is final and not type-specific to Direction
    private def lookup(i: Int): Direction = Directions(i).asInstanceOf[Direction]

    private def mod(x: Int, n: Int) = if (x >= 0) x % n else (x + n)
    private def bump: Int = bump(this.id)
    private def bump(v: Int): Int = if (v + 1 >= maxId) minId else v + 1
    private def decr: Int = if (this.id - 1 < minId) maxId - 1 else this.id - 1
    private def bump(v: Int, n: Int): Int = {
      require(n >= 0)
      var i = n
      var result = v
      while (i > 0) {
        result = bump(result)
        i = i - 1
      }
      return result
    }
  }

  /**
   * Like withName, but throws a NoSuchElementException with a meaningful message.
   */
  def forName(n: String): Direction = {
      values.find(_.toString == n) match {
        case Some(x) => x.asInstanceOf[Direction]
        case None => throw new java.util.NoSuchElementException(n)
      }
  }
}
