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
    for {
      sentence <- newSentences
      sent <- CNFConversion.convertUntilFixedPoint(sentence)
    } {
      if (!sent.isInstanceOf[Disjunction]) {
        throw new CNFConversionException(sent)
      }
      sentences.add(sent)
    }

  }

  def tell(sentence: Sentence) {
    tell(List(sentence))
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

  def as3CNF = {
    for (sentence <- sentences) yield CNFConversion.expandDisjunction(sentence.asInstanceOf[Disjunction])
  }
}
