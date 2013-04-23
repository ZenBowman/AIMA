package org.zenbowman.linearprogramming

import scala.collection.mutable.ListBuffer

class LPLang {
  var primaryEquation: Option[Equation] = None
  var constraints: List[Constraint] = List()
  var generatedVariableCounter = 0

  case class Equation(variables: List[VariableHelper]) {
    def +(vh: VariableHelper) = Equation(variables :+ vh)
    def -(vh: VariableHelper) = Equation(variables :+ VariableHelper(-vh.num, vh.name))
    def maximize() = primaryEquation = Some(this)
    def <= (i: Int) = constraints = Constraint(this, i, '<=) :: constraints
  }

  case class Constraint(equation: Equation, rightHandSide: Int, constraintType: Symbol)

  case class VariableHelper(num: Int, name: String) {
    def +(vh: VariableHelper) = Equation(List(this)).+(vh)
    def -(vh: VariableHelper) = Equation(List(this)).-(vh)
  }

  case class CoefficientHelper(num: Int) {
    def apply(variable: LinearVariable) = VariableHelper(num, variable.name)
  }

  case class LinearVariable(name: String)
  val x = LinearVariable("x")
  val y = LinearVariable("y")
  val z = LinearVariable("z")

  type Coefficient = Int
  implicit def intToCoefficient(coefficient: Coefficient) = CoefficientHelper(coefficient)

  def generateVariable = {
    val slackVariable = VariableHelper(1, "genvar%s".format(generatedVariableCounter))
    generatedVariableCounter += 1
    slackVariable
  }

  def solve = {
    val initialTable = generate
    val finalTable = SimplexSolver.solve(initialTable)
    finalTable.draw()
  }

  def getCoefficientRow(varList: List[VariableHelper]) = {
    val coefficientRow = new ListBuffer[Double]
    for (vh <- varList) {
      coefficientRow.append(vh.num.toDouble)
    }
    new LPRow(coefficientRow.toList)
  }

  def getCoefficientFor(variableName: String, equation: Equation): Double = {
    for (vh <- equation.variables) {
      if (vh.name == variableName) {
        return vh.num.toDouble
      }
    }
    0.0
  }

  def getRow(varList: List[VariableHelper], equation: Equation, rhs: Int) = {
    val coefficients = new ListBuffer[Double]
    for (vh <- varList) {
      coefficients.append(getCoefficientFor(vh.name, equation))
    }
    coefficients.append(rhs.toDouble)
    new LPRow(coefficients.toList)
  }

  def getRowList(varList: List[VariableHelper], rList: List[(Equation, Int)]) =  {
    val rows = new ListBuffer[LPRow]
    for ((equation, rightHandSide) <- rList) {
      rows.append(getRow(varList, equation, rightHandSide))
    }
    new LPRowSet(rows.toList)
  }

  def getPosition(variableList: List[VariableHelper], bv: String): Option[Int] = {
    for (i <- 0 until variableList.length; vh = variableList(i)) {
      if (vh.name == bv) {
        return Some(i)
      }
    }
    None
  }

  def getBaseVars(variableList: List[VariableHelper], baseVariables: List[String]) = {
    val baseVars = new ListBuffer[Int]
    for (bv <- baseVariables) {
      baseVars.append(getPosition(variableList, bv).get)
    }
    baseVars.toList
  }

  def generate = {
    val (variableList, rowList, baseVariables) = addSlack
    val coefficientRow = getCoefficientRow(variableList)
    val rList = getRowList(variableList, rowList)
    val baseVars = getBaseVars(variableList, baseVariables)
    new LPTable(coefficientRow, rList, baseVars)
  }

  def addSlack = {
    val variableList = new ListBuffer[VariableHelper]
    val rowList = new ListBuffer[(Equation, Int)]
    val baseVariables = new ListBuffer[String]

    for (vh <- primaryEquation.get.variables) variableList.append(vh)
    for (constraint <- constraints) {
      val inequality  = constraint.equation
      if (constraint.constraintType == '<=) {
        val newVariable = generateVariable
        rowList.append((inequality + newVariable, constraint.rightHandSide))
        variableList.append(VariableHelper(0, newVariable.name))
        baseVariables.append(newVariable.name)
      }
    }
    (variableList.toList, rowList.toList, baseVariables.toList)
  }
}
