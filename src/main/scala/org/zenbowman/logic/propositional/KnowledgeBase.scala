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

  }

  def tell(sentence: Sentence) {
    tell(List(sentence))
  }

  def ask(sentence: Sentence) = {
    if (sentences.contains(ExpandedDisjunction(List(sentence)))) {
      True
    } else if (sentences.contains(ExpandedDisjunction(List(Negation(sentence))))) {
      False
    } else {
      Unknown
    }
  }
}
