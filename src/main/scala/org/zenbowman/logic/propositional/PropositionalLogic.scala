package org.zenbowman.logic.propositional

object PropositionalLogic {

  trait Sentence {
    def ^ (s2: Sentence) = {
      Conjunction(this, s2)
    }

    def -> (s2: Sentence) = {
      Implication(this, s2)
    }

    def v (s2: Sentence) = {
      Disjunction(this, s2)
    }

    def <-> (s2: Sentence) = {
      BiConditional(this, s2)
    }
  }

  case object not {
    def apply(s1: Sentence) = Negation(s1)
  }

  trait AtomicSentence extends Sentence

  case class SymbolSentence(value: Symbol) extends AtomicSentence

  case class BooleanSentence(value: Boolean) extends AtomicSentence

  trait ComplexSentence extends Sentence

  case class Negation(sentence: Sentence) extends ComplexSentence

  case class Conjunction(sent1: Sentence, sent2: Sentence) extends ComplexSentence

  case class Disjunction(sent1: Sentence, sent2: Sentence) extends ComplexSentence

  case class Implication(premise: Sentence, consequent: Sentence) extends ComplexSentence

  case class BiConditional(sent1: Sentence, sent2: Sentence) extends ComplexSentence

  implicit def symbolToSentence(symbol: Symbol) = SymbolSentence(symbol)

  implicit def symbolsToSentences(symbols: Seq[Symbol]) = for (symbol <- symbols) yield SymbolSentence(symbol)

  case object True

  case object False

  case object Unknown

}

