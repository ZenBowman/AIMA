package org.zenbowman.linearprogramming

import junit.framework.{Assert, TestCase}

class LPTest extends TestCase("LPTest") {

  def genProblem1: LPTable = {
    val coefficientRow = new LPRow(List(6, 5, 0, 0))
    val row1 = new LPRow(List(1, 1, 1, 0, 5))
    val row2 = new LPRow(List(3, 2, 0, 1, 12))
    val rowSet = new LPRowSet(List(row1, row2))
    new LPTable(coefficientRow, rowSet, List(2, 3))
  }

  def printWithSep(x: String) {
    println("----------------------------------")
    println(x)
    println("----------------------------------")
  }

  def testProblemCreation() {
    val lpProblem = genProblem1
    assert(lpProblem != null)
  }

  def testCjMinusZj() {
    val lpProblem = genProblem1
    val result = lpProblem.generateCjMinusZj
    Assert.assertEquals(List(6, 5, 0, 0), result)
  }

  def testPivotColumnSelection() {
    val lpProblem = genProblem1
    val pivCol = lpProblem.selectPivotColumn
    Assert.assertEquals(Some(0), pivCol)
  }

  def testPivotRowSelection() {
    val lpProblem = genProblem1
    val pivCol = lpProblem.selectPivotColumn
    val pivRow = lpProblem.selectPivotRow(pivCol.get)
    Assert.assertEquals(1, pivRow)
  }

  def testIteration() {
    val lpProblem = genProblem1
    printWithSep("Initial state")
    lpProblem.draw()
    val nextProblemState = lpProblem.iterate
    Assert.assertTrue(nextProblemState.isDefined)
    printWithSep("After pivot")
    nextProblemState.get.draw()
  }
}

