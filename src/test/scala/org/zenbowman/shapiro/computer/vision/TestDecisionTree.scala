package org.zenbowman.shapiro.computer.vision

import junit.framework.{Assert, TestCase}
import org.zenbowman.shapiro.computer.vision.DecisionTreeTypes.{OutcomeSpec}


class TestDecisionTree extends TestCase {
  val epsilon = 0.05 // allowance for floating point imprecision

  /*
   * Test taken from exercise 4.2 of Shapiro's computer vision book
   * Testing probabilities and entropy measure
   */
  def testDecisionTreeEntropyFunction() {
    import org.zenbowman.shapiro.computer.vision.exercises.Exercise_4_2._

    Assert.assertEquals(1.0 / 8, outcomeBuckets(outcome1), epsilon)
    Assert.assertEquals(3.0 / 4, outcomeBuckets(outcome2), epsilon)
    Assert.assertEquals(1.0 / 16, outcomeBuckets(outcome3), epsilon)
    Assert.assertEquals(1.0 / 16, outcomeBuckets(outcome4), epsilon)

    Assert.assertEquals(1.06, DecisionTreeTypes.entropyOf(List(OutcomeSpec(outcome1, 0.75),
      OutcomeSpec(outcome2, 0.125), OutcomeSpec(outcome3, 0.125))), epsilon)
  }

  /*
   * Test taken from figure 4.12 of Shapiro's computer vision book
   * Validates the feature selection part of decision tree learning
   */
  def testInformationGain() {
    import org.zenbowman.shapiro.computer.vision.exercises.Figure_4_11._

    println("Entropy of original set = " + originalSet.entropy)

    val possiblePartitions = List(originalSet.partitionToList(0),
      originalSet.partitionToList(1), originalSet.partitionToList(2))

    for (part <- possiblePartitions) {
      println(DecisionTreeTypes.entropySum(part))
    }

    // Verifies that Quinlan's measure for information gain matches what is shown in
    // the book
    Assert.assertEquals(0.311, originalSet.informationContent(0), epsilon)
    Assert.assertEquals(1.0, originalSet.informationContent(1), epsilon)
    Assert.assertEquals(0.0, originalSet.informationContent(2), epsilon)
  }

  /*
   * Verifying decision tree construction using
   * figure 4.12
   *
   */
  def testDecisionTreeBuilding() {
    val set_4_11 = org.zenbowman.shapiro.computer.vision.exercises.Figure_4_11.originalSet
    val dt = DecisionTreeBuilder.buildDecisionTree(set_4_11)
    println(dt)


    val set_4_11m = org.zenbowman.shapiro.computer.vision.exercises.Figure_4_11_modified.originalSet
    val dt2 = DecisionTreeBuilder.buildDecisionTree(set_4_11m)
    println(dt2)

  }
}