package org.zenbowman.shapiro.computer.vision

import junit.framework.{Assert, TestCase}
import org.zenbowman.shapiro.computer.vision.DecisionTree.{DecisionTreeSet, OutcomeSpec}


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

    Assert.assertEquals(1.06, DecisionTree.entropyOf(List(OutcomeSpec(outcome1, 0.75),
      OutcomeSpec(outcome2, 0.125), OutcomeSpec(outcome3, 0.125))), epsilon)
  }

  /*
   * Test taken from figure 4.12 of Shapiro's computer vision book
   * Validates the feature selection part of decision tree learning
   */
  def testInformationGain() {
    val class1: DecisionTree.Outcome = List(true, false)
    val class2: DecisionTree.Outcome = List(false, true)

    val sample1 = DecisionTree.Sample(List(true, true, true), class1)
    val sample2 = DecisionTree.Sample(List(true, true, false), class1)
    val sample3 = DecisionTree.Sample(List(false, false, true), class2)
    val sample4 = DecisionTree.Sample(List(true, false, false), class2)

    val originalSet = DecisionTreeSet(List(sample1, sample2, sample3, sample4))

    println("Entropy of original set = " + originalSet.entropy)

    val possiblePartitions = List(originalSet.partition(0),
      originalSet.partition(1), originalSet.partition(2))

    for (part <- possiblePartitions) {
      println(DecisionTree.entropySum(part))
    }

  }
}
