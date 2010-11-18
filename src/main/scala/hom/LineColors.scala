
package hom

import java.awt.Color

/**
 * Light colors, supporting combinations of RGB.
 */
object LineColors extends Enumeration { 
  val Red = LineColor("Red", true, false, false)
  val Green = LineColor("Green", false, true, false)
  val Blue = LineColor("Blue", false, false, true)
  val Yellow = LineColor("Yellow", true, true, false)
  val Magenta = LineColor("Magenta", true, false, true)
  val Cyan = LineColor("Cyan", false, true, true)
  val White = LineColor("White", true, true, true)
  val Black = LineColor("Black", false, false, false)

  private def LineColor(s: String, r: Boolean, g: Boolean, b: Boolean) = new LineColor(s, r, g, b)

  class LineColor private[LineColors] (i: Int, s: String, val r: Boolean, val g: Boolean, val b: Boolean) extends Val(i, s) {
    private[LineColors] def this(s: String, r: Boolean, g: Boolean, b: Boolean) = this(nextId, s, r, g, b)

    /** The corresponding java.awt.Color value. */
    val color = new Color(v(r),v(g),v(b))

    def components: List[LineColor] = {
      var result: List[LineColor] = Nil
      if (r) result = Red :: result
      if (g) result = Green :: result
      if (b) result = Blue :: result
      result
    }

    def +(other: LineColor) = withRGB(this.r || other.r, this.g || other.g, this.b || other.b)

    private def v(on: Boolean) = if (on) 255 else 0
  }

  def colors = values.toList.map(_.asInstanceOf[LineColor])
  
  def withRGB(r: Boolean, g: Boolean, b: Boolean): LineColor = colors.find(c => c.r == r && c.g == g && c.b == b).get

  def withNameIgnoreCase(s: String): LineColor = values.find(_.toString.equalsIgnoreCase(s)).get.asInstanceOf[LineColor]
}
