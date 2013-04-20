package org.zenbowman.linearprogramming

class LPTable(val coefficientRow: LPRow, val rowSet: LPRowSet, val baseVars: List[Int]) {

  def value: Double = {
    var sum = 0.0
    for (i <- 0 until baseVars.length) {
      val rhs = rowSet(i).last
      val coefficient = coefficientRow(baseVars(i))
      sum += rhs * coefficient
    }
    sum
  }

  def draw() {
    println("Coefficients:")
    coefficientRow.draw()
    println("Rows:")
    rowSet.draw()
    println("Base variables:")
    println(baseVars)
    println("Value = %s".format(value))
  }

  def generateCjMinusZj: List[Double] = {
    val resultBuffer = coefficientRow.toBuffer
    for (i <- 0 until resultBuffer.length) {
      for (bv <- 0 until baseVars.length) {
        val correspondingRow = rowSet(bv)
        val coefficient = coefficientRow(baseVars(bv))
        resultBuffer(i) = resultBuffer(i) - (coefficient * correspondingRow(i))
      }
    }
    resultBuffer.toList
  }

  def doPivot(pivotColumn: Int, pivotRow: Int): LPTable = {
    val rowToPivot = rowSet(pivotRow)
    val divisor = rowToPivot(pivotColumn)
    val newPivotRow = rowToPivot.divideBy(divisor)
    val newRowSet = rowSet.applyPivot(newPivotRow, pivotRow, pivotColumn)
    val newBaseVars = baseVars.toBuffer
    newBaseVars(pivotRow) = pivotColumn
    new LPTable(coefficientRow, newRowSet, newBaseVars.toList)
  }

  def iterate: Option[LPTable] = {
    val pivotColumn = selectPivotColumn
    if (!pivotColumn.isDefined) {
      return None
    }
    val pivotRow = selectPivotRow(pivotColumn.get)
    Some(doPivot(pivotColumn.get, pivotRow))
  }

  def solved = !selectPivotColumn.isDefined

  def selectPivotColumn: Option[Int] = {
    val row = generateCjMinusZj
    var max = 0.0
    var colMax: Option[Int] = None
    for (i <- 0 until row.length; v = row(i)) {
      if (v > max) {
        max = v
        colMax = Some(i)
      }
    }
    colMax
  }

  def selectPivotRow(pivotColumn: Int): Int = {
    var min = 0.0
    var minRow: Option[Int] = None
    for (bv <- 0 until baseVars.length) {
      val correspondingRow = rowSet(bv)
      val rhs = correspondingRow.last
      val candidate = rhs / (correspondingRow(pivotColumn))
      if (!minRow.isDefined || (candidate < min)) {
        min = candidate
        minRow = Some(bv)
      }
    }
    minRow.get
  }
}

