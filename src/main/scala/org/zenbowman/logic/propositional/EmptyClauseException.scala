package org.zenbowman.logic.propositional

import org.zenbowman.logic.propositional.PropositionalLogic.ExpandedDisjunction

class EmptyClauseException(val s1: ExpandedDisjunction, val s2: ExpandedDisjunction) extends Exception{
  override def toString: String = {
    "%s and %s resolves to an empty clause".format(s1, s2)
  }
}
