package org.zenbowman.logic.firstorder

object FirstOrderLogic {

  trait Sentence {
    def and(s2: Sentence) = {
      Conjunction(this, s2)
    }

    def ->(s2: Sentence) = {
      Implication(this, s2)
    }

    def or(s2: Sentence) = {
      Disjunction(this, s2)
    }

    def <->(s2: Sentence) = {
      BiConditional(this, s2)
    }
  }

  trait AtomicSentence extends Sentence

  case class Predicate(name: String) {
    def apply(terms: Term*) = {
      PredicatePhrase(this, terms)
    }
  }

  trait Term

  case class Function(name: String, arity: Int)

  case class PredicatePhrase(predicate: Predicate, terms: Seq[Term]) extends AtomicSentence

  case class EqualityPhrase(equal: Boolean, lhs: Term, rhs: Term) extends AtomicSentence

  case class FunctionPhrase(function: Function, terms: Seq[Term]) extends Term

  case class Constant(name: String) extends Term

  case class Variable(name: String) extends Term

  trait Quantifier {
    def apply(x: Symbol) = {
      QuantifierPhrase(this, Variable(x.name))
    }
  }

  case object ForEach extends Quantifier

  case object Exists extends Quantifier

  case class QuantifierPhrase(quantifier: Quantifier, variable: Variable) {
    def apply(s: Sentence) = {
      QuantifiedSentence(this, s)
    }
  }

  case class QuantifiedSentence(quantifierPhrase: QuantifierPhrase, sentence: Sentence) extends Sentence

  trait Connective

  case class Negation(sent: Sentence) extends Sentence

  case class Conjunction(sent1: Sentence, sent2: Sentence) extends Sentence

  case class Disjunction(sent1: Sentence, sent2: Sentence) extends Sentence

  case class Implication(premise: Sentence, consequent: Sentence) extends Sentence

  case class BiConditional(sent1: Sentence, sent2: Sentence) extends Sentence

  implicit def symbolToConstant(symbol: Symbol) = Constant(symbol.name)

  // TODO: how to define wireless separately

}
