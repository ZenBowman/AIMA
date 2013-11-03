package org.zenbowman.logic.propositional

import org.zenbowman.logic.propositional.PropositionalLogic.Sentence


class CNFConversionException (val sentence: Sentence) extends Exception {
  override def toString: String = {
    "Unable to convert sentence into conjunctive normal form: %s".format(sentence)
  }
}
