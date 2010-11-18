package hom

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import LineColors._

class LineColorsTest extends AssertionsForJUnit {

  @Test
  def redIsRed() {
    expect("Red") {
      LineColors.Red.toString
    }
    expect(Red) {
      LineColors.withNameIgnoreCase("red")
    }
  }

  @Test
  def magentaIsRedAndBlue() {
    expect(Magenta) {
      Red + Blue
    }
  }
  

}

