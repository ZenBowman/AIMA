package org.zenbowman.linearprogramming

object SimplexSolver {

  def solve(tableState: LPTable): LPTable = {
    val x = tableState.iterate
    if (x.isDefined) {
      return solve(x.get)
    }
    tableState
  }

}
