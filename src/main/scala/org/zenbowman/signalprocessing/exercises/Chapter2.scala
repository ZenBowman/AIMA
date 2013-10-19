package org.zenbowman.signalprocessing.exercises

import org.zenbowman.signalprocessing._


object Chapter2 extends ExerciseHelper {

  def printAndDisplay(differenceEquation: DEquation,
                      func: Int => Float = FilterCreator.impulse,
                      number: Int = 10) {
    val result = FilterCreator.generateResponse(differenceEquation, func, number)
    for (i <- result) {
      Console.print(i + "  ")
    }
  }

  def exercise_2_1_a() {
    printExerciseName("2.1(a)")
    val differenceEquation = new DEquation(
      xs = List(1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f),
      ys = List())
    printAndDisplay(differenceEquation)
  }

  def exercise_2_1_c() {
    printExerciseName("2.1(c)")
    val differenceEquation = new DEquation(
      xs = List(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, -1.0f),
      ys = List(0.0f, 1.0f)
    )
    printAndDisplay(differenceEquation)
  }

  def exercise_2_6() {
    printExerciseName("2.6")
    val differenceEquation = new DEquation(
      xs = List(1.0f, -1.9021f, 1.0f),
      ys = List(0.0f, 1.8523f, -0.94833f)
    )
    printAndDisplay(differenceEquation)
  }

  def exercise_2_9_a() {
    printExerciseName("2.9(a)")
    printAndDisplay(
      differenceEquation = new DEquation(
        xs = List(1.0f), ys = List(0.0f, 0.5f)
      ),
      func = FilterCreator.step, number = 10)
  }

  def exercise_2_9_b() {
    printExerciseName("2.9(b)")
    printAndDisplay(
      differenceEquation = new DEquation(
        xs = List(1.0f), ys = List(0.0f, -0.5f)
      ),
      func = FilterCreator.step, number = 10)

    printExerciseName("2.9(b) with new API")
    val de = new DifferenceEquation(List(
      DifferenceComponent(DConstants.X, 1.0f, 0),
      DifferenceComponent(DConstants.Y, -0.5f, -1)
    ))
    val response = de.generate(FilterCreator.step, 0 until 10)
    for (item <- response) {
      Console.print(item + "  ")
    }
  }

  def exercise_2_10_a() {
    printExerciseName("2.10(a)")
    val filter = new Filter(List(1.0f, 1.0f, 1.0f))
    val signal = new Signal(List(0.0f, 0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f))
    val result = filter.convolve(signal)
    for (item <- result.values) {
      Console.print(item + "  ")
    }
  }


  def exercise_2_10_b() {
    printExerciseName("2.10(b)")
    val filter = new Filter(List(1.0f, -1.0f))
    val signal = new Signal(List(1.0f, 2.0f, 0.5f, -0.5f, -1, -0.2f, 0.1f))
    val result = filter.convolve(signal)
    for (item <- result.values) {
      Console.print(item + "  ")
    }
  }

  def exercise_2_12() {
    printExerciseName("2.12")
    val de = new DifferenceEquation(List(
      DifferenceComponent(DConstants.X, 1.0f / 7, -3),
      DifferenceComponent(DConstants.X, 1.0f / 7, -2),
      DifferenceComponent(DConstants.X, 1.0f / 7, -1),
      DifferenceComponent(DConstants.X, 1.0f / 7, 0),
      DifferenceComponent(DConstants.X, 1.0f / 7, 1),
      DifferenceComponent(DConstants.X, 1.0f / 7, 2),
      DifferenceComponent(DConstants.X, 1.0f / 7, 3)
    ))
    val signal = new Signal(List(9, 10, 8, 10, 12, 11, 9, 11, 10, 16, 18, 18, 14, 22, 12, 15, 11, 10, 9, 10,
      16, 17, 18, 14, 9, 11, 7, 8, 9, 11))
    val result = de.generate(signal.get, 0 until 25)
    SignalPlotter.plot("2.12", result)
    Console.println(result(0) + "\t" + result(6) + "\t" + result(20))
  }

  def exercise_2_13() {
    printExerciseName("2.13")
    val de = new DifferenceEquation(List(
      DifferenceComponent(DConstants.X, 2.0f, 0),
      DifferenceComponent(DConstants.X, -3.0f, -1),
      DifferenceComponent(DConstants.X, 5.0f, -2),
      DifferenceComponent(DConstants.X, -5.0f, -3),
      DifferenceComponent(DConstants.X, 3.0f, -4),
      DifferenceComponent(DConstants.X, -2.0f, -5)
    ))
    val impulseResponse = de.generate(FilterCreator.impulse, 0 until 20)
    val stepResponse = de.generate(FilterCreator.step, 0 until 20)

    SignalPlotter.plot("2.13 - impulse", impulseResponse)
    SignalPlotter.plot("2.13 - step", stepResponse)

    Console.print("\n Impulse response = [")
    for (i <- impulseResponse) Console.print(i + " ")
    Console.print("]\n Step response = [")
    for (s <- stepResponse) Console.print(s + " ")
    Console.println("]")
  }

  def exercise_2_18() {
    printExerciseName("2.18")
    val de = new DifferenceEquation(List(
      DifferenceComponent(DConstants.X, 0.2f, 0),
      DifferenceComponent(DConstants.Y, 0.8f, -1)
    ))
    val result = de.generate(FilterCreator.step, 0 until 50)
    for (i <- 0 until 50) {
      Console.print(result(i) + " ")
      if (i % 10 == 0) {
        Console.println()
      }
    }
  }

  def main(args: Array[String]) {
    val sinusoid = for (i <- 0 until 20) yield scala.math.sin(i * (scala.math.Pi/4)).toFloat * 5
    SignalPlotter.plot("Sinusoid", sinusoid)
    exercise_2_1_a()
    exercise_2_1_c()
    exercise_2_6()
    exercise_2_9_a()
    exercise_2_9_b()
    exercise_2_10_a()
    exercise_2_10_b()
    exercise_2_12()
    exercise_2_13()
    exercise_2_18()
    SignalPlotter.show()
  }
}
