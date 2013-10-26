package org.zenbowman.shapiro.computer.vision

import scala.collection.mutable.ListBuffer
import org.zenbowman.shapiro.computer.vision.DecisionTreeTypes.{OutcomeSpec, Sample}

case class FeatureSpecification(featureIndex: Int, featureValue: Boolean)

class DecisionTreeNode(val featureSpec: Option[FeatureSpecification], val set: DecisionTreeSet) {
  private val children = new ListBuffer[DecisionTreeNode]

  def evaluateSample(sample: DecisionTreeTypes.Features): OutcomeSpec = {
    for (child <- children) {
      if (child.featureSpec.isDefined) {
        val spec = child.featureSpec.get
        if (sample(spec.featureIndex) == spec.featureValue) {
          return child.evaluateSample(sample)
        }
      }
    }

    var maxOutcome = OutcomeSpec(null, 0.0)
    for (outcome <- set.possibleOutcomes) {
      if (outcome.probability > maxOutcome.probability) {
        maxOutcome = outcome
      }
    }
    maxOutcome
  }

  def addChild(dt: DecisionTreeNode) {
    children.append(dt)
  }

  def toString(depth: Int): String = {
    val sb = new StringBuilder
    for(fs <- featureSpec) {
      for (i <- 0 until depth) sb.append("\t")
      sb.append("IF feature at index %d = %s:\n".format(fs.featureIndex, fs.featureValue))
    }

    for (i <- 0 until depth) sb.append("\t")
    sb.append("Outcome likelihood = %s\n".format(set.toString))

    for (child <- children) {
      sb.append("%s".format(child.toString(depth + 1)))
    }

    sb.toString()
  }

  override def toString: String = {
    toString(0)
  }
}
