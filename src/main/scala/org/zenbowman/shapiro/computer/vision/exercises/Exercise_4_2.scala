package org.zenbowman.shapiro.computer.vision.exercises

import org.zenbowman.shapiro.computer.vision.{DecisionTreeSet, DecisionTreeTypes}
import org.zenbowman.shapiro.computer.vision.DecisionTreeTypes.{Sample}

object Exercise_4_2 {
  val featureless: DecisionTreeTypes.Features = List()
  val outcome1: DecisionTreeTypes.Outcome = List(true, false)
  val outcome2: DecisionTreeTypes.Outcome = List(true, true)
  val outcome3: DecisionTreeTypes.Outcome = List(false, false)
  val outcome4: DecisionTreeTypes.Outcome = List(false, true)

  val sample1 = Sample(featureless, outcome1)
  val sample2 = Sample(featureless, outcome2)
  val sample3 = Sample(featureless, outcome3)
  val sample4 = Sample(featureless, outcome4)

  val decisionTreeSpec = DecisionTreeSet(List(sample1, sample1,
    sample2, sample2, sample2, sample2, sample2, sample2, sample2,
    sample2, sample2, sample2, sample2, sample2, sample3, sample4
  ))

  val outcomeBuckets = decisionTreeSpec.possibleOutcomes.map(x => (x.outcome, x.probability)).toMap

  def main(args: Array[String]) {
    println("Answer to exercise 4.2 = " + decisionTreeSpec.entropy)
  }
}
