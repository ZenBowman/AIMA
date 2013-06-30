package org.zenbowman.signalprocessing


class Signal(val values: List[Float]){


  def apply(i: Int) = get(i)

  def get(i: Int) = {
    if ((i < 0) || (i > values.length)) {
      0
    } else {
      values(i)
    }
  }

  def length = values.length

  def scalarMultiply(v: Float) = {
    for (sample <- values) yield sample * v
  }

  override def toString: String = values.toString()
}
