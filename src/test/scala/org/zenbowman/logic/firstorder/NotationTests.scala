package org.zenbowman.logic.firstorder

import junit.framework.TestCase

class NotationTests extends TestCase {
  import FirstOrderLogic._

  def testSimpleNotation() {
    val brother = Predicate("Brother")
    val s = brother('john, 'james) and brother('james, 'john)
    println(s)
  }

  def testQuantifier() {
    val failed = Predicate("Failed")
    val s = Exists('x)(failed('x, 'biology))
    println(s)
  }
}
