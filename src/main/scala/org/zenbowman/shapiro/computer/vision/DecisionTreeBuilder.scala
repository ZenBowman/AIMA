package org.zenbowman.shapiro.computer.vision

object DecisionTreeBuilder {

  val MINIMUM_INFORMATION_GAIN: Double = 0.1

  def getBestFeature(set: DecisionTreeSet, numFeatures: Int) = {
    var maxIndex = -1
    var max: Double = -1
    for (i <- 0 until numFeatures) {
      val ic = set.informationContent(i)
      if (ic > max) {
        max = ic
        maxIndex = i
      }
    }
    (maxIndex, max)
  }

  def buildDecisionTreeFromNode(node: DecisionTreeNode, numberOfFeatures: Int): DecisionTreeNode = {
    val (currentBestFeatureIndex, informationGain) = getBestFeature(node.set, numberOfFeatures)

    if (informationGain > MINIMUM_INFORMATION_GAIN) {
      val (left, right) = node.set.partition(currentBestFeatureIndex)
      val leftChild = new DecisionTreeNode(Some(FeatureSpecification(currentBestFeatureIndex, true)), left)
      val rightChild = new DecisionTreeNode(Some(FeatureSpecification(currentBestFeatureIndex, false)), right)

      buildDecisionTreeFromNode(leftChild, numberOfFeatures)
      buildDecisionTreeFromNode(rightChild, numberOfFeatures)

      node.addChild(leftChild)
      node.addChild(rightChild)
    }

    node
  }

  def buildDecisionTree(originalSet: DecisionTreeSet) = {
    val firstSample = originalSet.samples.headOption
    if (!firstSample.isDefined) {
      throw new Exception("The samples in the original set have no features")
    }
    val numberOfFeatures = firstSample.get.features.length
    val headNode = new DecisionTreeNode(None, originalSet)

    buildDecisionTreeFromNode(headNode, numberOfFeatures)
  }

}
