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
    kb.tell('breeze_1_3 ^ 'breeze_2_3)  // Testing the special syntax
    Assert.assertEquals(True, kb.ask('breeze_1_2))
    Assert.assertEquals(True, kb.ask('breeze_2_2))
    Assert.assertEquals(True, kb.ask('breeze_2_3))
    Assert.assertEquals(Unknown, kb.ask('breeze_3_3))
  }

  def testNegationSyntax {
    val kb = new KnowledgeBase
    kb.tell(not('breeze_1_2))


  }

  def testDeMorgans() {
    val kb = new KnowledgeBase
    kb.tell(Negation('a ^ 'b))
    Assert.assertEquals(True, kb.ask(Negation('a) v Negation('b)))
  }
}
