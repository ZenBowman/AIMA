package org.zenbowman.shapiro.computer.vision

import scala.collection.mutable

object DecisionTree {
  type BinaryFeature = Boolean
  type BinaryOutput = Boolean
  type Features = List[BinaryFeature]
  type Outcome = List[BinaryOutput]

  case class Sample(features: Features, outcome: Outcome)

  case class OutcomeSpec(outcome: Outcome, probability: Double)


  def logBase2(x: Double) = math.log(x) / math.log(2)


  def entropyOf(outcomes: Iterable[OutcomeSpec]) = {
    val entropies = for {
      outcome <- outcomes
      probability = outcome.probability
    }
    yield -1 * probability * logBase2(probability)

    entropies.sum
  }

  case class DecisionTreeSet(samples: List[Sample]) {

    def possibleOutcomes: Iterable[OutcomeSpec] = {
      val outcomes = new mutable.HashMap[Outcome, Int]()

      for (sample <- samples) {
        if (outcomes.contains(sample.outcome)) {
          outcomes(sample.outcome) += 1
        } else {
          outcomes.put(sample.outcome, 1)
        }
      }

      for ((outcome, number) <- outcomes) yield OutcomeSpec(outcome, number.toDouble / samples.length)
    }

    def entropy: Double = {
      entropyOf(possibleOutcomes)
    }

    def numAppearancesOfFeature(featureIndex: Int, featureValue: Boolean): Int = {
      samples.filter(_.features(featureIndex) == featureValue).size
    }

    def numAppearancesOfOutcomeAndFeature(outcome: Outcome, featureIndex: Int, featureValue: Boolean): Int = {
      samples.filter(x => (x.features(featureIndex) == featureValue) && x.outcome == outcome).size
    }

    def probabilityOfFeature(featureIndex: Int, featureValue: Boolean): Double = {
      numAppearancesOfFeature(featureIndex, featureValue).toDouble / samples.length
    }

    def probabilityOfOutcomeAndFeature(outcome: Outcome, featureIndex: Int, featureValue: Boolean): Double = {
      numAppearancesOfOutcomeAndFeature(outcome, featureIndex, featureValue).toDouble / samples.length
    }

    def partialInformationContentFor(possibleOutcome: OutcomeSpec, featureIndex: Int, featureValue: Boolean): Double = {
      val jointProbability = probabilityOfOutcomeAndFeature(possibleOutcome.outcome, featureIndex, featureValue)
      val productOfProbabilities = possibleOutcome.probability * probabilityOfFeature(featureIndex, featureValue)
      val jointOverProduct = jointProbability / productOfProbabilities
      if (jointProbability == 0) {
        0
      } else {
        jointProbability * logBase2(jointOverProduct)
      }

    }

    def informationContentFor(possibleOutcome: OutcomeSpec, featureIndex: Int) = {
      val pWhenFeature = partialInformationContentFor(possibleOutcome, featureIndex, true)
      val pWhenNotFeature = partialInformationContentFor(possibleOutcome, featureIndex, false)
      pWhenFeature + pWhenNotFeature
    }

    // The information content of the outcome with respect to the feature in position featureIndex
    def informationContent(featureIndex: Int) = {
      (for (possibleOutcome <- possibleOutcomes) yield
        informationContentFor(possibleOutcome, featureIndex)).sum
    }

    def partition(featureIndex: Int): Iterable[DecisionTreeSet] = {
      val sampleSets = samples.partition(x => x.features(featureIndex))
      List(DecisionTreeSet(sampleSets._1), DecisionTreeSet(sampleSets._2))
    }
  }

  def entropySum(sets: Iterable[DecisionTreeSet]): Double = {
    (for (set <- sets) yield set.entropy).sum
  }
}