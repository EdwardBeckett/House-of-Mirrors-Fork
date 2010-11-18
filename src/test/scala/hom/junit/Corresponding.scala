
package hom.junit

class CorrespondenceFailure[A <: Any, B <: Any] (detail: AnyRef) extends AssertionError(detail) {

  def this(i: Int, a: Seq[Any], b: Seq[Any]) {
    this(xformat("Seqs differ at " + i, element(i,a), element(i,b)))
    def element(x: Int, s: Seq[Any]): Any = if (x < s.length) s(x) else "No element"
    def xformat(message: String, expected: Any, actual: Any): String = {
      val header: String = if (message != null && !message.equals("")) message + ", " else ""
      val expectedString = String.valueOf(expected)
      val actualString = String.valueOf(actual)
      if (expectedString.equals(actualString)) header + "expected:" + formatClassAndValue(expected, expectedString) + " but was:" + formatClassAndValue(actual, actualString)
      else header + "expected:<" + expectedString + "> but was:<" + actualString + ">"
    }
    def formatClassAndValue(v: Any, s: String): String = {
      v match {
        case null => "null"
        case x: AnyRef => x.getClass.getName + "<" + s + ">"
        case _ => "Value " + v
      }
    }
  }

}
