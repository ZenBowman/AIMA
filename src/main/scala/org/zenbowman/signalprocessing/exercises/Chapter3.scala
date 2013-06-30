package org.zenbowman.signalprocessing.exercises

object Chapter3 extends ExerciseHelper {

  val epsilon = 0.000001

  def round(num: Double) = if (scala.math.abs(num) < epsilon) 0 else num

  def isPeriodicIn(func: Int => Double, periodGuess: Int): Boolean = {
    for (i <- 0 until periodGuess * 3) {
      if (scala.math.abs(func(i) - func(i + periodGuess)) > epsilon) {
        return false
      }
    }
    true
  }

  def guessPeriod(func: Int => Double): Option[Int] = {
    for (periodGuess <- 2 until 20) {
      if (isPeriodicIn(func, periodGuess)) {
        return Some(periodGuess)
      }
    }
    None
  }

  def spectralCoefficientFor(harmonic: Int, func: Int => Double, period: Int) = {
    var realCoefficient = 0.0
    var imaginaryCoefficient = 0.0
    for (n <- 0 until period) {
      realCoefficient += func(n) * scala.math.cos((scala.math.Pi * 2 * harmonic * n) / period)
      imaginaryCoefficient -= func(n) * scala.math.sin((scala.math.Pi * 2 * harmonic * n) / period)
    }
    (round(realCoefficient / period), round(imaginaryCoefficient / period))
  }

  def signalA(n: Int) = {
    5 + scala.math.sin((n * scala.math.Pi) / 2) + scala.math.cos((n * scala.math.Pi) / 4)
  }

  def signalB(n: Int) = {
    scala.math.cos(((scala.math.Pi * n) / 2) - (scala.math.Pi / 4))
  }

  def calculateSpectral(func: Int => Double) {
    val period = guessPeriod(func)
    for {
      p <- period
      k <- 0 until p
    } {
      Console.println("A_%s = %s".format(k, spectralCoefficientFor(k, func, p)))
    }
  }

  def exercise_3_1() {
    printExerciseName("3.1(a)")
    calculateSpectral(signalA)

    printExerciseName("3.1(b)")
    calculateSpectral(signalB)

    val periodOfB = guessPeriod(signalB)
    Console.println("Period of B = " + periodOfB)
  }

  def exercise_3_2() {
    def signal(n: Int): Double = {
      val _n = n % 7
      _n match {
        case 0 => 2.0
        case 1 => -4.0
        case 2 => 1.0
        case 3 => -2.0
        case 4 => 3.0
        case 5 => -2.0
        case 6 => 2.0
      }
    }

    printExerciseName("3.2")
    calculateSpectral(signal)
  }

  def main(args: Array[String]) {
    exercise_3_1()
    exercise_3_2()
  }
}
