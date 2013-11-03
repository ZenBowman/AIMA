package org.zenbowman.logic.propositional

import scala.collection.mutable

object Resolution {

  import org.zenbowman.logic.propositional.PropositionalLogic._

  def unitResolve(d: Disjunction, m: Literal): ExpandedDisjunction = {
    val s = CNFConversion.expandDisjunction(d)
    val elems = new mutable.HashSet[Sentence]
    for {
      clause <- s.clauses if clause.isInstanceOf[Literal]
      l = clause.asInstanceOf[Literal]
    } {
      if (l.opposite != m) {
        elems.add(l)
      }
    }
    ExpandedDisjunction(elems.toSeq)
  }
}
