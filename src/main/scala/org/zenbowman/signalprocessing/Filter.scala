package org.zenbowman.signalprocessing


class Filter(impulseResponse: List[Float]) {
  def convolve(signal: Signal): Signal = {
    val newSignal = new Array[Float](signal.length + impulseResponse.length)
    for {
      pos <- 0 until signal.length
      sample = signal.get(pos)
      i <- 0 until impulseResponse.length
      resp = impulseResponse(i)
      n = pos + i
    } {
      newSignal.update(n, newSignal(n) + sample * resp)
    }
    new Signal(newSignal.toList)
  }
}

object DConstants {
  val X = 0
  val Y = 1
}

case class DifferenceComponent(typ: Int, coefficient: Float, deltaN: Int)

class DifferenceEquation(components: List[DifferenceComponent]) {
  def generate(signal: Int => Float, range: Range) = {
    val response = new Array[Float](range.length)
    def getResponse(n: Int) = if (n > 0 && n < response.length) response(n) else 0.0f
    for (i <- range) {
      var sum = 0.0f
      for {
        comp <- components
        n = i + comp.deltaN
      } {
        comp.typ match {
          case DConstants.X =>
            sum += comp.coefficient * signal(n)
          case DConstants.Y =>
            sum += comp.coefficient * getResponse(n)
          case _ =>
            throw new Exception("Unknown component type")
        }
      }
      response(i) = sum
    }
    response
  }
}

@Deprecated
class DEquation(xs: List[Float], ys: List[Float]) {
  def maxLength = scala.math.max(xs.length, ys.length)

  def x(n: Int) = if (n >= xs.length) 0 else xs(n)

  def y(n: Int) = if (n >= ys.length) 0 else ys(n)
}

object FilterCreator {

  def generateImpulseResponse(differenceEquation: DEquation, number: Int = 10) = {
    generateResponse(differenceEquation, impulse, number)
  }

  def generateStepResponse(differenceEquation: DEquation, number: Int = 10) = {
    generateResponse(differenceEquation, step, number)
  }

  def step(n: Int) = if (n > 0) 1.0f else 0.0f

  def impulse(n: Int) = if (n == 0) 1.0f else 0.0f

  def generateResponse(differenceEquation: DEquation, signal: Int => Float, number: Int) = {
    val len = number
    val response = new Array[Float](number)

    for (c <- 0 until len) {
      var sum = 0.0f
      for (x_i <- 0 until c + 1) {
        val x_c = c - x_i
        sum += signal(x_c) * differenceEquation.x(x_i)
      }
      for (y_i <- 0 until c + 1) {
        val y_c = c - y_i
        sum += response(y_c) * differenceEquation.y(y_i)
      }
      response(c) = sum
    }
    response
  }
}
