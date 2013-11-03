package org.zenbowman.logic.propositional

import scala.collection.mutable

class KnowledgeBase {

  import org.zenbowman.logic.propositional.PropositionalLogic._

  private var sentences = new mutable.HashSet[Sentence]

  def dump() {
    for (sent <- sentences) {
      println(sent.toString)
    }
    println("-------")
  }

  def tell(newSentences: Seq[Sentence]) {
    for (sentence <- newSentences) {
      sentences.add(sentence)
    }
    runInferences()
  }

  def tell(sentence: Sentence) {
    tell(List(sentence))
  }

  def runInferences() {
    var oldSnapshot: List[Sentence] = List()
    var newSnapshot: List[Sentence] = List()
    do {
      oldSnapshot = sentences.toList
      applyInferenceRules()
      newSnapshot = sentences.toList
    } while (oldSnapshot != newSnapshot)
  }

  def applyInferenceRules() {
    dump()
    for (sentence <- sentences) {
      val newSentences = PropositionalLogicRules.convert(sentence)
      if (newSentences.isDefined) {
        sentences.remove(sentence)
        for {
          newSentencesV <- newSentences
          sentence <- newSentencesV
        } {
          sentences.add(sentence)
        }
      }
    }
  }

  def ask(sentence: Sentence) = {
    if (sentences.contains(sentence)) {
      True
    } else if (sentences.contains(Negation(sentence))) {
      False
    } else {
      Unknown
    }

  }
}
