package org.zenbowman.logic.propositional

import junit.framework.{Assert, TestCase}

class TestResolution extends TestCase {
  def testUnitResolution() {
    import PropositionalLogic._

    val dj1 = ExpandedDisjunction(List(not('breeze_1_1), 'breeze_1_2))
    val lit1 = 'breeze_1_1
    val dj2 = Resolution.unitResolve(dj1, lit1)
    Assert.assertEquals(ExpandedDisjunction(List('breeze_1_2)), dj2)
  }
}
