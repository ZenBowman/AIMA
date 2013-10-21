package org.zenbowman.shapiro.computer.vision.exercises

import org.zenbowman.shapiro.computer.vision.{DecisionTreeSet, DecisionTreeTypes}

object Figure_4_11 {
  val class1: DecisionTreeTypes.Outcome = List(true, false)
  val class2: DecisionTreeTypes.Outcome = List(false, true)

  val sample1 = DecisionTreeTypes.Sample(List(true, true, true), class1)
  val sample2 = DecisionTreeTypes.Sample(List(true, true, false), class1)
  val sample3 = DecisionTreeTypes.Sample(List(false, false, true), class2)
  val sample4 = DecisionTreeTypes.Sample(List(true, false, false), class2)

  val originalSet = DecisionTreeSet(List(sample1, sample2, sample3, sample4))

}

object Figure_4_11_modified {
  val class1: DecisionTreeTypes.Outcome = List(true, false)
  val class2: DecisionTreeTypes.Outcome = List(false, true)

  val sample1 = DecisionTreeTypes.Sample(List(true, true, true), class1)
  val sample2 = DecisionTreeTypes.Sample(List(true, true, false), class1)
  val sample3 = DecisionTreeTypes.Sample(List(false, false, true), class2)
  val sample4 = DecisionTreeTypes.Sample(List(true, false, false), class2)
  val sample5 = DecisionTreeTypes.Sample(List(true, false, true), class1)

  val originalSet = DecisionTreeSet(List(sample1, sample2, sample3, sample4, sample5))

}
