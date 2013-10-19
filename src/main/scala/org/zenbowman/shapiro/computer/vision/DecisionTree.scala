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

    def partition(featureIndex: Int): Iterable[DecisionTreeSet] = {
      val sampleSets = samples.partition(x => x.features(featureIndex))
      List(DecisionTreeSet(sampleSets._1), DecisionTreeSet(sampleSets._2))
    }
  }

  def entropySum(sets: Iterable[DecisionTreeSet]): Double = {
    (for (set <- sets) yield set.entropy).sum
  }
}