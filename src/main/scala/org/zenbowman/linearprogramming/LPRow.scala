package org.zenbowman.linearprogramming


class LPRow(values: List[Double]) {

  def apply(x: Int) = values(x)
  def toBuffer = values.toBuffer
  def length = values.length
  def last  = values.last

  def draw() {
    println(values)
  }

  def divideBy(divisor: Double) = {
    new LPRow(values.map(x => x/divisor))
  }

  def multiplyBy(multiplier: Double) = {
    new LPRow(values.map(x => x * multiplier))
  }

  def add(lprow: LPRow) = {
    val newValues = values.toBuffer
    for (i <- 0 until this.length) {
      newValues(i) += lprow(i)
    }
    new LPRow(newValues.toList)
  }

  def transformRowForPivot(pivotRow: LPRow, pivotColumn: Int) = {
    val elementToMakeZero = this(pivotColumn)
    val correspondingPivotRowElement = pivotRow(pivotColumn)
    val multiplier = (- elementToMakeZero)/correspondingPivotRowElement
    add(pivotRow.multiplyBy(multiplier))
  }
}
