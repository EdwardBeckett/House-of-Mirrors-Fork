package hom

object Days extends Enumeration {
  type Day = DayValue
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = /*new*/ DayValue

  private def DayValue: DayValue = new DayValue(nextId, if (nextName.hasNext) nextName.next else null)

  protected class DayValue(i: Int, s: String) extends Val(i, s) {
    //protected class DayValue extends Val {
    def isWeekday: Boolean = {
      this match {
        case Sun => false
        case Sat => false
        case _ => true
      }
    }
  }
}
