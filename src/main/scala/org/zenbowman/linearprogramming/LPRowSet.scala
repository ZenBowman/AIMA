package org.zenbowman.linearprogramming

import scala.collection.mutable.ListBuffer


class LPRowSet(rowList: List[LPRow]) {
  def apply(x: Int) = rowList(x)
  def length = rowList.length
  def draw() {
    for (r <- rowList) {
      r.draw()
    }
  }

  def applyPivot(newPivotRow: LPRow, pivotRow: Int, pivotColumn: Int) = {
    val newRowList = new ListBuffer[LPRow]
    for (i <- 0 until this.length) {
      if (i == pivotRow) {
        newRowList.append(newPivotRow)
      }
      else {
        newRowList.append(rowList(i).transformRowForPivot(newPivotRow, pivotColumn))
      }
    }
    new LPRowSet(newRowList.toList)
  }
}
