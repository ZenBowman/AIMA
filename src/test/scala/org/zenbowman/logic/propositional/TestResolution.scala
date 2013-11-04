package org.zenbowman.logic.propositional

import junit.framework.{Assert, TestCase}

class TestResolution extends TestCase {
  import PropositionalLogic._

  def testUnitResolution() {
    val dj1 = ExpandedDisjunction(Set(not('breeze_1_1), 'breeze_1_2))
    val lit1 = 'breeze_1_1
    val dj2 = Resolution.unitResolve(dj1, lit1)
    Assert.assertEquals(ExpandedDisjunction(Set('breeze_1_2)), dj2)
  }

  def testFullResolution() {
    val d1 = ExpandedDisjunction(Set('a, 'b, 'c))
    val d2 = ExpandedDisjunction(Set(not('a), 'd, 'e))

    val d3 = Resolution.resolve(d1, d2)
    Assert.assertEquals(1, d3.size)
    Assert.assertEquals(Set[Sentence]('b, 'c, 'e, 'd), d3.head.clauses)
  }

  def testKBResolution() {
    val kb = new KnowledgeBase
    kb.tell('a or 'b)
    kb.tell(not('a))
    kb.tell('b -> 'c)
    kb.tell('a <-> 'd)

    Assert.assertEquals(True, kb.ask('b))
    Assert.assertEquals(False, kb.ask('a))
    Assert.assertEquals(True, kb.ask('c))
    Assert.assertEquals(False, kb.ask('d))
  }
}
