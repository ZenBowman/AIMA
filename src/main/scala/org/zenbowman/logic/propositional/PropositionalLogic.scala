package org.zenbowman.logic.propositional

object PropositionalLogic {

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

  case object not {
    def apply(s1: Sentence) = Negation(s1)
  }

  trait AtomicSentence extends Sentence

  trait Literal extends Sentence {
    def opposite: Sentence
  }

  case class SymbolSentence(value: Symbol) extends AtomicSentence with Literal {
    override def toString: String = value.toString()

    def opposite = Negation(this)
  }

  trait ComplexSentence extends Sentence

  case class Negation(sentence: Sentence) extends ComplexSentence with Literal {
    override def toString: String = {
      "NOT(%s)".format(sentence)
    }

    def opposite = sentence
  }

  case class Conjunction(sent1: Sentence, sent2: Sentence) extends ComplexSentence {
    override def toString: String = {
      "(%s AND %s)".format(sent1, sent2)
    }
  }

  case class ExpandedDisjunction(clauses: Set[Sentence]) extends Sentence {
    override def toString: String = {
      clauses.mkString(" OR ")
    }
  }

  val EMPTY_CLAUSE = ExpandedDisjunction(Set())

  case class Disjunction(sent1: Sentence, sent2: Sentence) extends ComplexSentence {
    override def toString: String = {
      "(%s OR %s)".format(sent1, sent2)
    }
  }

  case class Implication(premise: Sentence, consequent: Sentence) extends ComplexSentence {
    override def toString: String = {
      "(%s => %s)".format(premise, consequent)
    }
  }

  case class BiConditional(sent1: Sentence, sent2: Sentence) extends ComplexSentence {
    override def toString: String = {
      "(%s <=> %s)".format(sent1, sent2)
    }
  }

  implicit def symbolToSentence(symbol: Symbol) = SymbolSentence(symbol)

  implicit def symbolsToSentences(symbols: Seq[Symbol]) = for (symbol <- symbols) yield SymbolSentence(symbol)

  implicit def literalToExpandedDisjunction(literal: Literal) = ExpandedDisjunction(Set(literal))

  trait TruthValue

  case object True extends TruthValue

  case object False extends TruthValue

  case object Unknown extends TruthValue

}

