package org.zenbowman.logic.propositional

import scala.collection.mutable

class KnowledgeBase {

  import org.zenbowman.logic.propositional.PropositionalLogic._

  private var sentences = new mutable.HashSet[ExpandedDisjunction]

  def dump() {
    for (sent <- sentences) {
      println(sent.toString)
    }
    println("-------")
  }

  def tell(newSentences: Seq[Sentence]) {
    for {
      sentence <- newSentences
      sent <- CNFConversion.convertUntilFixedPoint(sentence)
    } {
      sentences.add(CNFConversion.asExpandedDisjunction(sent))
    }
    val newCNFs = Resolution.resolution(sentences.toSet)
    sentences ++= newCNFs
  }

  def tell(sentence: Sentence) {
    tell(List(sentence))
  }

  def ask(l: Literal): TruthValue = {
    val d = CNFConversion.toExpandedDisjunction(l)
    val notD = CNFConversion.toExpandedDisjunction(l.opposite)
    if (sentences.contains(d)) {
      return True
    } else if (sentences.contains(notD)) {
      return False
    }

    Resolution.testResolution(sentences.toSet, notD)
  }
}
