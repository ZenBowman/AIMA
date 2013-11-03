package org.zenbowman.logic.propositional

import junit.framework.{Assert, TestCase}

class TestPropositionalLogic extends TestCase {

  import PropositionalLogic._

  def testSimpleTell() {
    val kb = new KnowledgeBase
    kb.tell('breeze_1_1)
    kb.tell(List('breeze_1_2, 'breeze_2_2))

    Assert.assertEquals(True, kb.ask('breeze_1_1))
    Assert.assertEquals(True, kb.ask('breeze_1_2))
    Assert.assertEquals(True, kb.ask('breeze_2_2))
    Assert.assertEquals(Unknown, kb.ask('breeze_2_3))
  }

  def testAndElimination() {
    val kb = new KnowledgeBase
    kb.tell(Conjunction('breeze_1_2, 'breeze_2_2))
    kb.tell('breeze_1_3 and 'breeze_2_3) // Testing the special syntax
    Assert.assertEquals(True, kb.ask('breeze_1_2))
    Assert.assertEquals(True, kb.ask('breeze_2_2))
    Assert.assertEquals(True, kb.ask('breeze_2_3))
    Assert.assertEquals(Unknown, kb.ask('breeze_3_3))
  }

  /*
   *  Tests both negation and DeMorgans law
   *  NOT (A OR B) === (NOT A) AND (NOT B)
   */
  def testNegation() {
    val kb = new KnowledgeBase
    kb.tell(not('breeze_1_2 or 'breeze_1_1))
    Assert.assertEquals(True, kb.ask(not('breeze_1_2)))
    Assert.assertEquals(False, kb.ask('breeze_1_1))
  }

  def testDeMorgans() {
    val kb = new KnowledgeBase
    kb.tell(not('a or 'b))
    Assert.assertEquals(True, kb.ask(not('a)))
  }
}


