package org.zenbowman.shapiro.computer.vision

object DecisionTreeTypes {
  type BinaryFeature = Boolean
  type BinaryOutput = Boolean
  type Features = List[BinaryFeature]
  type Outcome = List[BinaryOutput]

  case class Sample(features: Features, outcome: Outcome)

  case class OutcomeSpec(outcome: Outcome, probability: Double) {
    override def toString: String = {
      "class%s(p=%s)".format(outcome.indexOf(true), probability)
    }
  }

  def logBase2(x: Double) = math.log(x) / math.log(2)

  def entropyOf(outcomes: Iterable[OutcomeSpec]) = {
    val entropies = for {
      outcome <- outcomes
      probability = outcome.probability
    }
    yield -1 * probability * logBase2(probability)

    entropies.sum
  }

  def entropySum(sets: Iterable[DecisionTreeSet]): Double = {
    (for (set <- sets) yield set.entropy).sum
  }

}