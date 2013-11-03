package org.zenbowman.logic.propositional

import scala.collection.mutable.ListBuffer
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

  def distribute(c: Conjunction, s: Sentence) {
    tell(Disjunction(c.sent1, s))
    tell(Disjunction(c.sent2, s))
  }

  def deMorgan(n: Negation) = {
    n match {
      case Conjunction(s1, s2) => {
        Disjunction(Negation(s1), Negation(s2))
      }
      case Disjunction(s1, s2) => {
        Conjunction(Negation(s1), Negation(s2))
      }
      case _ => n
    }
  }

  def applyInferenceRules() {
    dump()
    for (sentence <- sentences) {
      sentence match {
        // And-elimination
        case Conjunction(s1, s2) => {
          println("Applying And-elimination: %s".format(sentence))
          sentences -= sentence
          tell(List(s1, s2))
        }

        // DeMorgan's Law 1
        case Negation(Conjunction(s1, s2)) => {
          println("Applying DeMorgan's law #1: %s".format(sentence))
          sentences -= sentence
          tell(Disjunction(Negation(s1), Negation(s2)))
        }

        // DeMorgan's Law 2
        case Negation(Disjunction(s1, s2)) => {
          println("Applying DeMorgan's law #2: %s".format(sentence))
          sentences -= sentence
          tell(Conjunction(Negation(s1), Negation(s2)))
        }

        // Bi-conditional elimination
        case BiConditional(s1, s2) => {
          println("Applying biconditional elimination: %s".format(sentence))
          sentences -= sentence
          tell(Conjunction(Implication(s1, s2), Implication(s2, s1)))
        }

        case Implication(s1, s2) => {
          println("Expanding implication: %s".format(sentence))
          sentences -= sentence
          tell(Disjunction(Negation(s1), s2))
        }

        // Double-negation elimination
        case Negation(Negation(s)) => {
          println("Eliminating double-negation: %s".format(sentence))
          sentences -= sentence
          tell(s)
        }

        case Disjunction(Conjunction(s1, s2), s3) => {
          println("Distributing disjunction: %s".format(sentence))
          sentences -= sentence
          distribute(Conjunction(s1, s2), s3)
        }

        case Disjunction(s1, Conjunction(s2, s3)) => {
          println("Distributing disjunction: %s".format(sentence))
          sentences -= sentence
          distribute(Conjunction(s2, s3), s1)
        }

        case _ => ()
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
