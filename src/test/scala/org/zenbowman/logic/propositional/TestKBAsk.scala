package org.zenbowman.logic.propositional

import junit.framework.TestCase

class TestKBAsk extends TestCase {

  import org.zenbowman.logic.propositional.{PropositionalLogic, KnowledgeBase}
  import PropositionalLogic._

  def testSimpleAsk() {
    val kb = new KnowledgeBase

    kb.tell('a or 'b)
    kb.tell(not('a))
    kb.tell('b -> 'c)
    kb.tell('a <-> 'd)

    kb.ask('b)
  }
}
