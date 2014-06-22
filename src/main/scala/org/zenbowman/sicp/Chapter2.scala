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
      case "8" =>
        ExtendedExercise.exercise8()
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

  object ExtendedExercise {
    case class Interval(lowerBound: Double, upperBound: Double) {
      override def toString() = s"$lowerBound:$upperBound"
    }
    def lowerBound(i: Interval) = i.lowerBound
    def upperBound(i: Interval) = i.upperBound

    def makeInterval(a: Double, b: Double) = {
      Interval(a, b)
    }

    def subInterval(a: Interval, b: Interval) = {
      Interval(
        lowerBound(a) - upperBound(b),
        upperBound(a) - lowerBound(b)
      )
    }

    implicit def PairToInterval(pp : (Double, Double)) = {
      Interval(pp._1, pp._2)
    }

    def printBinaryFunction(a: Interval, b: Interval, 
      f:(Interval, Interval) => Interval, fname: String) {
      println(s"($fname $a $b) = ${f(a, b)}")
    }

    def printSub(a: Interval, b: Interval) {
      printBinaryFunction(a, b, subInterval, "sub-interval")
    }

    def exercise8() {
      printSub(Interval(3, 5), Interval(1, 1))
      printSub(Interval(3, 3), Interval(1, 4))
      printSub(Interval(5.0, 7.0), Interval(-5.5, 20.5))
    }

  }


}
