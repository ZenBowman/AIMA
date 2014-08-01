package org.zenbowman.pfds.chapter2

import junit.framework.{Assert, TestCase}

class UnbalancedSetTest extends TestCase {
  def testBasicSetOperations() {
    val emptySet: UnbalancedSet[V] = EmptySet()
    Assert.assertFalse(emptySet.member(V(1)))
  }
}
