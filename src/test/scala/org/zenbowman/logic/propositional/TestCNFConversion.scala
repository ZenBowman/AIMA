package org.zenbowman.logic.propositional

import junit.framework.TestCase

class TestCNFConversion extends TestCase {
  import PropositionalLogic._

  def testCNFConversion() {
    val kb = new KnowledgeBase
    kb.tell('breeze_1_1 <-> ('pit_1_2 v 'pit_2_1))
    kb.dump()
  }

}
