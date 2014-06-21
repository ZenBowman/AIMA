package org.zenbowman.sicp

import scala.math

object Chapter2 {


  def apply(exercise: String) {
    println("=========")
    println("Chapter 2")
    println(s"Exercise $exercise")
    println("=========")

    exercise match {
      case "1" =>
        Exercise1()
      case _ =>
        println("Unknown exercise")
    }
  }

  object Exercise1 {
    case class RationalNumber(numerator: Int, denominator: Int) {
      override def toString() = {
        s"$numerator / $denominator"
      }
    }

    def numer(rat: RationalNumber) = rat.numerator
    def denom(rat: RationalNumber) = rat.denominator

    def makeRat(num: Int, denom: Int): RationalNumber = {
      if ((num >= 0) && (denom >= 0)) {
        RationalNumber(num, denom)
      } else if ((num < 0) && (denom < 0)) {
        RationalNumber(math.abs(num), math.abs(denom))
      } else {
        RationalNumber(-math.abs(num), math.abs(denom))
      }
    }

    def printRat(num: Int, denom: Int) {
      println(s"(make-rat $num $denom) = ${makeRat(num, denom)}")
    }

    def apply() {
      printRat(3, 5)
      printRat(-2, -3)
      printRat(5, -7)
    }
  }
}
