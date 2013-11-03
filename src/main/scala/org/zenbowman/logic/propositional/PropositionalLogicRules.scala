package org.zenbowman.logic.propositional

import org.zenbowman.logic.propositional.PropositionalLogic._
import scala.Some

object PropositionalLogicRules {
  def distribute(c: Conjunction, s: Sentence) = {
    List(Disjunction(c.sent1, s), Disjunction(c.sent2, s))
  }

  def deMorgan(s: Sentence) = {
    s match {
      case Negation(Conjunction(s1, s2)) => {
        Disjunction(Negation(s1), Negation(s2))
      }
      case Negation(Disjunction(s1, s2)) => {
        Conjunction(Negation(s1), Negation(s2))
      }
      case _ => s
    }
  }

  def convert(sentence: Sentence): Option[Seq[Sentence]] = {
    sentence match {
      // And-elimination
      case Conjunction(s1, s2) => {
        println("Applying And-elimination: %s".format(sentence))
        Some(List(s1, s2))
      }

      // DeMorgan's Law 1
      case Negation(Conjunction(s1, s2)) => {
        println("Applying DeMorgan's law #1: %s".format(sentence))
        Some(List(deMorgan(sentence)))
      }

      // DeMorgan's Law 2
      case Negation(Disjunction(s1, s2)) => {
        println("Applying DeMorgan's law #2: %s".format(sentence))
        Some(List(deMorgan(sentence)))
      }

      // Bi-conditional elimination
      case BiConditional(s1, s2) => {
        println("Applying biconditional elimination: %s".format(sentence))
        Some(List(Conjunction(Implication(s1, s2), Implication(s2, s1))))
      }

      case Implication(s1, s2) => {
        println("Expanding implication: %s".format(sentence))
        Some(List(Disjunction(Negation(s1), s2)))
      }

      // Double-negation elimination
      case Negation(Negation(s)) => {
        println("Eliminating double-negation: %s".format(sentence))
        Some(List(s))
      }

      case Disjunction(Conjunction(s1, s2), s3) => {
        println("Distributing disjunction: %s".format(sentence))
        Some(for (s <- distribute(Conjunction(s1, s2), s3)) yield s)
      }

      case Disjunction(s1, Conjunction(s2, s3)) => {
        println("Distributing disjunction: %s".format(sentence))
        Some(for (s <- distribute(Conjunction(s2, s3), s1)) yield s)
      }

      case Disjunction(Negation(s1: ComplexSentence), s2) => {
        println("Applying DeMorgan's within disjunction: %s".format(sentence))
        Some(List(Disjunction(deMorgan(Negation(s1)), s2)))
      }

      case Disjunction(s2, Negation(s1: ComplexSentence)) => {
        println("Applying DeMorgan's within disjunction: %s".format(sentence))
        Some(List(Disjunction(deMorgan(Negation(s1)), s2)))
      }

      case _ => None
    }
  }

}
