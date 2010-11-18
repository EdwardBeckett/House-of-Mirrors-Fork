
package hom

import scala.xml._

/** Attributes in xml constant expressions are in reverse order from the code text,
 * so this PrettyPrinter re-reverses them for printing */
//class PrettierPrinter(w: Int, t: Int) extends PrettyPrinter(w, t) {
class PrettierPrinter(w: Int, t: Int) extends XMLFormatter(w, t) {

  def this() = this(120, 2)

  /*
  // this isn't sufficient, because it's not invoked for children that fit using Utility.
  override protected def leafTag(n: Node) = {
    def _mkLeaf(sb: StringBuilder) {
      sb append '<'
      n nameToString sb
      formatAttributes(n, sb)
      sb append "/>"
    }
    withStringBuilder(_mkLeaf)
  }
  override protected def startTag(n: Node, pscope: NamespaceBinding): (String, Int) = {
    var i = 0
    def _mkStart(sb: StringBuilder) {
      sb append '<'
      n nameToString sb
      i = sb.length + 1
      formatAttributes(n, sb)
      n.scope.buildString(sb, pscope)
      sb append '>'
    }
    (withStringBuilder(_mkStart), i)
  }
  private def withStringBuilder(f: (StringBuilder) => Unit): String = {
    val b = new StringBuilder
    f(b)
    b.toString
  }
  */
  // extension point in XMLFormatter
  override
  protected def formatAttributes(n: Node, sb: StringBuilder) {
    //n.attributes.toList.reverse.foldLeft(sb) { (a,b) => a.append(' '); b.toString1(a); a }
    (sb /: n.attributes.toList.reverse) { (r,v) => r.append(' '); v.toString1(r); r }
  }
}

