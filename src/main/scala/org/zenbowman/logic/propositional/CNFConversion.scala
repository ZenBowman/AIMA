package org.zenbowman.logic.propositional

import org.zenbowman.logic.propositional.PropositionalLogic._
import scala.Some
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object CNFConversion {
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

  def convertUntilFixedPoint(sentence: Sentence): Seq[Sentence] = {
    var oldSnapshot: mutable.HashSet[Sentence] = new mutable.HashSet[Sentence]()
    var newSnapshot: mutable.HashSet[Sentence] = new mutable.HashSet[Sentence]()
    newSnapshot.add(sentence)
    do {
      oldSnapshot.clear()
      for (elem <- newSnapshot) {
        oldSnapshot.add(elem)
      }
      newSnapshot.clear()
      for {
        sent <- oldSnapshot
        elem <- convert(sent)
      } {
        newSnapshot.add(elem)
      }
    } while (oldSnapshot.toList != newSnapshot.toList)

    newSnapshot.toList
  }

  def convert(sentence: Sentence): List[Sentence] = {
    sentence match {
      // And-elimination
      case Conjunction(s1, s2) => {
        println("Applying And-elimination: %s".format(sentence))
        List(s1, s2)
      }

      // DeMorgan's Law 1
      case Negation(Conjunction(s1, s2)) => {
        println("Applying DeMorgan's law #1: %s".format(sentence))
        List(deMorgan(sentence))
      }

      // DeMorgan's Law 2
      case Negation(Disjunction(s1, s2)) => {
        println("Applying DeMorgan's law #2: %s".format(sentence))
        List(deMorgan(sentence))
      }

      // Bi-conditional elimination
      case BiConditional(s1, s2) => {
        println("Applying biconditional elimination: %s".format(sentence))
        List(Conjunction(Implication(s1, s2), Implication(s2, s1)))
      }

      case Implication(s1, s2) => {
        println("Expanding implication: %s".format(sentence))
        List(Disjunction(Negation(s1), s2))
      }

      // Double-negation elimination
      case Negation(Negation(s)) => {
        println("Eliminating double-negation: %s".format(sentence))
        List(s)
      }

      case Disjunction(Conjunction(s1, s2), s3) => {
        println("Distributing disjunction: %s".format(sentence))
        for (s <- distribute(Conjunction(s1, s2), s3)) yield s
      }

      case Disjunction(s1, Conjunction(s2, s3)) => {
        println("Distributing disjunction: %s".format(sentence))
        for (s <- distribute(Conjunction(s2, s3), s1)) yield s
      }

      case Disjunction(Negation(s1: ComplexSentence), s2) => {
        println("Applying DeMorgan's within disjunction: %s".format(sentence))
        List(Disjunction(deMorgan(Negation(s1)), s2))
      }

      case Disjunction(s2, Negation(s1: ComplexSentence)) => {
        println("Applying DeMorgan's within disjunction: %s".format(sentence))
        List(Disjunction(deMorgan(Negation(s1)), s2))
      }

      case _ => List(sentence)
    }
  }

  def disjunctionAsElements(s: Sentence): List[Sentence] = {
    s match {
      case Disjunction(s1, s2) => List(s1, s2)
      case _ => List(s)
    }
  }

  /*
   *  Given a disjunction of the form (a or (b or c))
   *  Returns the sentence (a or b or c)
   *  This is used for converting to 3-CNF after all the conversion rules have been applied
   */
  def expandDisjunction(d: Disjunction): ExpandedDisjunction = {
    ExpandedDisjunction((for {
      element <- disjunctionAsElements(d)
      innerElement <- disjunctionAsElements(element)
    } yield innerElement).toSet)
  }

  def asExpandedDisjunction(s: Sentence): ExpandedDisjunction = {
    s match {
      case d: Disjunction => expandDisjunction(d)
      case l: Literal => ExpandedDisjunction(Set(l))
      case _ => throw new CNFConversionException(s)
    }
  }

  /*
   *  Currently only works correctly for literals
   */
  def toExpandedDisjunction(s: Sentence): ExpandedDisjunction = {
    asExpandedDisjunction(convertUntilFixedPoint(s).head)
  }
}
