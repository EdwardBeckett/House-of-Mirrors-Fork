package hom.junit

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.Seq

class CorrespondingTest extends AssertionsForJUnit {

  @Test def comparingTwoLists() {
    val i = 3
    val a = List(0,1,2,3,4)
    val b = List(0,1,2,7,4)
    val f = new hom.junit.CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 3, expected:<3> but was:<7>") {
      f.getMessage();
    }
  }

  @Test def comparingListsDifferentSizes() {
    val i = 3
    val a = List(0,1,2,3,4)
    val b = List(0,1,2)
    val f = new hom.junit.CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 3, expected:<3> but was:<No element>") {
      f.getMessage();
    }
  }

  @Test def comparingListsDifferentSizesReverse() {
    val i = 3
    val a = List(0,1,2,3,4)
    val b = List(0,1,2)
    val f = new hom.junit.CorrespondenceFailure(i,b.asInstanceOf[Seq[AnyRef]],a.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 3, expected:<No element> but was:<3>") {
      f.getMessage();
    }
  }

  @Test def comparingListsDifferentTypesAndStrings() {
    val i = 0
    val a = List(new A)
    val b = List(new B)
    val f = new hom.junit.CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 0, expected:<2> but was:<3>") {
      f.getMessage();
    }
  }

  @Test def comparingListsDifferentTypesAndSameStrings() {
    val i = 0
    val a = List(new A)
    val b = List(new B(2))
    val f = new hom.junit.CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 0, expected:hom.junit.CorrespondingTest$A<2> but was:hom.junit.CorrespondingTest$B<2>") {
      f.getMessage();
    }
  }

  @Test def comparingListsMixedPrimitives() {
    val i = 0
    val a = List(new A)
    val b = List(2)
    val f = new hom.junit.CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 0, expected:hom.junit.CorrespondingTest$A<2> but was:java.lang.Integer<2>") {
      f.getMessage();
    }
  }

  @Test def comparingListsDifferentTypesAndBothNulls() {
    val i = 0
    val a = List(new A)
    val b = List(null)
    val f = new hom.junit.CorrespondenceFailure(i,a.asInstanceOf[Seq[AnyRef]],b.asInstanceOf[Seq[AnyRef]])
    expect("Seqs differ at 0, expected:<2> but was:<null>") {
      f.getMessage();
    }
  }

  class A(val f:Int = 2) {
    override def toString = f.toString
  }

  class B(val f:Int = 3) {
    override def toString = f.toString
  }

}
