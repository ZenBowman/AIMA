package org.zenbowman.logic.propositional

import scala.collection.mutable.ListBuffer

class KnowledgeBase {

  import org.zenbowman.logic.propositional.PropositionalLogic._

  private var sentences = new ListBuffer[Sentence]

  def tell(newSentences: Seq[Sentence]) {
    for (sentence <- newSentences) {
      sentences.append(sentence)
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
    for (sentence <- sentences) {
      sentence match {
        // And-elimination
        case Conjunction(s1, s2) => {
          sentences -= sentence
          tell(List(s1, s2))
        }

        // DeMorgan's Law 1
        case Negation(Conjunction(s1, s2)) => {
          sentences -= sentence
          tell(Disjunction(Negation(s1), Negation(s2)))
        }

        // DeMorgan's Law 2
        case Negation(Disjunction(s1, s2)) => {
          sentences -= sentence
          tell(Conjunction(Negation(s1), Negation(s2)))
        }

        case _ => ()
      }
    }
  }

  def ask(sentence: Sentence) = {
    if (sentences.contains(sentence)) {
      True
    } else {
      Unknown
    }

  }
}
