package hom

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Base(val provenance: String)

trait Useful

trait Useless

class Special(p: String) extends Base(p) with Useful

class AnotherSpecial(p: String) extends Base(p) with Useful

class Ordinary(p: String) extends Base(p)

class LessThanOrdinary(p: String) extends Base(p) with Useless

class NotSpecial

class ImplTest extends AssertionsForJUnit {

  @Test
  def simple() {
    implicit def toSpecial(x: NotSpecial): Special = new Special("From NS")
    val ns = new NotSpecial
    val s: Special = ns
    expect("From NS") { s.provenance }
  }

  @Test
  def dubious() {
    implicit def toSpecial(x: NotSpecial): Base with Useful = new Special("From Base with Useful")
    val ns = new NotSpecial
    val s: Base with Useful = ns
    expect("From Base with Useful") { s.provenance }
  }

  @Test
  def dubiouser() {
    implicit def toSpecial[T <: Base with Useful](x: NotSpecial): T = new Special("From <: Base with Useful").asInstanceOf[T]
    val ns = new NotSpecial
    val s: Special = ns
    expect("From <: Base with Useful") { s.provenance }
  }

  @Test
  def ambiguous() {
    implicit def toOrdinary(x: NotSpecial): Ordinary = new Ordinary("To Ordinary")
    implicit def toSpecial[T <: Base with Useful](x: NotSpecial): T = new Special("From <: Base with Useful").asInstanceOf[T]
    val ns = new NotSpecial
    val s: Special = ns
    expect("From <: Base with Useful") { s.provenance }
  }

  // doesn't compile because two implicits provide conversion to Base
//    @Test
//  def otherway() {
//    implicit def toOrdinary(x: NotSpecial): Ordinary = new Ordinary("To Ordinary")
//    implicit def toSpecial[T <: Base with Useful](x: NotSpecial): T = new Special("From <: Base with Useful").asInstanceOf[T]
//    val ns = new NotSpecial
//    val o: Ordinary = ns
//    expect("From <: Base with Useful") { o.provenance }
//  }

//      @Test
//  def notdisambiguated() {
//    implicit def toLTOrdinary(x: NotSpecial): LessThanOrdinary = new LessThanOrdinary("To LessThanOrdinary")
//    implicit def toSpecial[T <: Base with Useful](x: NotSpecial): T = new Special("From <: Base with Useful").asInstanceOf[T]
//    val ns = new NotSpecial
//    val o: LessThanOrdinary = ns
//    expect("From <: Base with Useful") { o.provenance }
//  }
      
//            @Test
//  def disambiguated() {
//    implicit def toLTOrdinary(x: NotSpecial): LessThanOrdinary = new LessThanOrdinary("To LessThanOrdinary")
//    implicit def toSpecial[T <: Base with Useful](x: NotSpecial): T = new Special("From <: Base with Useful").asInstanceOf[T]
//    val ns = new NotSpecial
//    val o: LessThanOrdinary = ns
//    expect("From <: Base with Useful") { o.provenance }
//  }
            
//                        @Test
//  def disambiguated() {
//    implicit def toLTOrdinary[T <: Base with Useless](x: NotSpecial): T = new LessThanOrdinary("To LessThanOrdinary").asInstanceOf[T]
//    implicit def toSpecial[T <: Base with Useful](x: NotSpecial): T = new Special("From <: Base with Useful").asInstanceOf[T]
//    val ns = new NotSpecial
//    val o: LessThanOrdinary = ns
//    expect("From <: Base with Useful") { o.provenance }
//  }
}

