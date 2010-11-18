package hom

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import Directions._

class HouseNotificationTest extends AssertionsForJUnit {

  @Test def mustIncludeStartup() {
    expect(Some(hom.notes.HouseNotifications.Startup)) {
      hom.notes.HouseNotifications.values.find(_.toString == "Startup")
    }
  }
  
  @Test def loadPackBodyIsTuple2() {
	  val sut = hom.notes.LoadGamePackNotification("foo", 2)
	  assert(sut.body.isInstanceOf[Tuple2[AnyRef,Int]])
	  val (what, level) = sut.body
	  expect(2) { level }
	  expect("foo") { what }
  }
  
  @Test def northIsNorth() {
    expect(North.id) {
      Directions.withName("North").id
    }
    expect(North) {
      Directions.forName("North")
    }
    expect(North) {
      North match {
        case North => North
        case _ => fail("Not North!")
      }
    }
  }
  
  @Test def right45OfNorthIsNortheast() {
    expect(Northeast){
      North.right45
    }
  }
  
  @Test def right90OfNorthIsEast() {
    expect(East){
      North.right90
    }
  }
  
  @Test def reverseOfNorthIsSouth() {
    expect(South){
      North.reverse
    }
  }
  
  @Test def reverseOfEastIsWest() {
    expect(West){
      East.reverse
    }
  }

  @Test def reverseOfSouthWestIsNorthEast() {
    expect(Northeast){
      Southwest.reverse
    }
  }
  
  @Test def canLookUpNorthByName() {
    expect(Some(North)) {
      Directions.values.find(_.toString == "North")
    }
    expect(Some(Northeast)) {
      Directions.values.find(_.toString == "Northeast")
    }
  }
  
  @Test def enumElementShouldHaveBehavior() {
    import Days._
    val d: Day = Mon
    expect (true) {
      d.isWeekday
    }
    val s = Sun
    expect (false) {
      s.isWeekday
    }
  }
  
  //org.scalatest.junit.JUnitTestFailedError: Expected Array(Mon, Tue, Wed, Thu, Fri, Sat, Sun), but got Array(4, 5, Mon, 6, 1, 2, 3)
  //@Test
  def badlyOrdered_dayElementsShouldBeNamed() {
    expect(Array("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) {
      val a = new Array[String](7)
      Days.values.collect{case d:Days.Value => d.toString}.copyToArray(a)
      a
    }
  }
  
  @Test def nameOfMon() {
    import Days._
    expect("Mon") {
      val d: Day = Mon
      d.toString
    }
  }
  
  //org.scalatest.junit.JUnitTestFailedError: Expected "[Tue]", but got "[1]"
  @Test def nameOfTue() {
    import Days._
    expect("Tue") {
      val d: Day = Tue
      d.toString
    }
  }
  
  //org.scalatest.junit.JUnitTestFailedError: Expected Array(Mon, Tue, Wed, Thu, Fri, Sat, Sun), but got Array(Mon, 1, 2, 3, 4, 5, 6)
  @Test def dayElementsShouldBeNamed() {
    val expected = Array("Mon","Tue","Wed","Thu","Fri","Sat","Sun");
    val actual = Days.values.toList.map(x => x.toString).toArray
    for (i <- 0 until expected.length) {
      expect(expected(i)) {
        actual(i);
      }
    }
  }
}

object HouseNotificationTest {
  
  def main(args: Array[String]) {
    Days.values.foreach(x => println(x.toString))
  }

}

