package org.zenbowman.shapiro.computer.vision

import org.zenbowman.shapiro.computer.vision.DecisionTreeTypes.Sample
import scala.collection.mutable.ListBuffer
import scala.collection.mutable


object SampleBinarizer {

  /*
   * Given: a list of samples, each of which is represented by a list of integers
   * Returns: a new list of samples, where each feature is represented by a boolean variable
   *          and each class of outcomes is represented by a list of booleans
   */
  def binarizeSamples(elements: List[List[Int]]) = {
    if (elements.size == 0) {
      throw new Exception("No samples in list")
    }
    val first = elements(1)
    if (first.size <= 1) {
      throw new Exception("Samples must have at least 1 feature and 1 outcome class")
    }
    val numFeatures = first.size - 1
    val possibleValues = Array.fill[mutable.HashSet[Int]](numFeatures)(new mutable.HashSet[Int]())
    val possibleOutcomes = new mutable.HashSet[Int]

    for (element <- elements) {
      for (i <- 0 until numFeatures) {
        possibleValues(i).add(element(i))
      }
      possibleOutcomes.add(element(numFeatures))
    }

    (possibleValues, possibleOutcomes)
  }
}
