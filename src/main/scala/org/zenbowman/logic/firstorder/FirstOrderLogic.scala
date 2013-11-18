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

  trait Term {
    def ===(t2: Term) = EqualityPhrase(equal = true, this, t2)

    def =!=(t2: Term) = EqualityPhrase(equal = false, this, t2)
  }

  case class Function(name: String, arity: Int)

  case class PredicatePhrase(predicate: Predicate, terms: Seq[Term]) extends AtomicSentence {
    override def toString: String = {
      val termString = terms.mkString(", ")
      s"${predicate.name}($termString)"
    }
  }

  case class EqualityPhrase(equal: Boolean, lhs: Term, rhs: Term) extends AtomicSentence {
    override def toString = {
      if (equal) {
        s"$lhs = $rhs"
      } else {
        s"$lhs ≠ $rhs"
      }
    }
  }

  case class FunctionPhrase(function: Function, terms: Seq[Term]) extends Term

  case class Constant(name: String) extends Term {
    override def toString = name
  }


  //TODO: In order to do unification, a variable MUST belong to a sentence
  //   Otherwise an 'x' in one sentence will be thought of as the same
  //   as an 'x' in another sentence
  //   Any instantiated variable must also have a quantifier type
  case class Variable(name: String) extends Term {
    override def toString = name
  }

  trait Quantifier {
    def apply(x: Variable) = {
      QuantifierPhrase(this, x)
    }
  }

  case object ForAll extends Quantifier {
    override def toString = "∀"
  }

  case object Exists extends Quantifier {
    override def toString = "Э"
  }

  case class QuantifierPhrase(quantifier: Quantifier, variable: Variable) {
    def apply(s: Sentence) = {
      QuantifiedSentence(this, s)
    }

    override def toString = s"$quantifier$variable"
  }

  case class QuantifiedSentence(quantifierPhrase: QuantifierPhrase, sentence: Sentence) extends Sentence {
    override def toString = {
      s"$quantifierPhrase $sentence"
    }
  }

  trait Connective

  case class Negation(sent: Sentence) extends Sentence

  case class Conjunction(sent1: Sentence, sent2: Sentence) extends Sentence {
    override def toString = {
      s"$sent1 ∧ $sent2"
    }
  }

  case class Disjunction(sent1: Sentence, sent2: Sentence) extends Sentence {
    override def toString = {
      s"$sent1 ∨ $sent2"
    }
  }

  case class Implication(premise: Sentence, consequent: Sentence) extends Sentence {
    override def toString = {
      s"$premise ⇒ $consequent"
    }
  }

  case class BiConditional(sent1: Sentence, sent2: Sentence) extends Sentence {
    override def toString: String = {
      s"$sent1 ⇔ $sent2"
    }
  }

  implicit def symbolToConstant(symbol: Symbol) = Constant(symbol.name)
}
