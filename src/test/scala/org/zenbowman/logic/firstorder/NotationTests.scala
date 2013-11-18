package org.zenbowman.logic.firstorder

import junit.framework.{Assert, TestCase}

class NotationTests extends TestCase {
  import FirstOrderLogic._

  def testSimpleNotation() {
    val brother = Predicate("brother")
    val s = brother('john, 'james) and brother('james, 'john)
    Assert.assertEquals("brother(john, james) ∧ brother(james, john)", s.toString)
  }

  def testQuantifiers() {
    val failed = Predicate("failed")
    val x = Variable("x")
    val y = Variable("y")
    val s = Exists(x)(failed(x, 'biology))
    Assert.assertEquals("Эx failed(x, biology)", s.toString)

    val s2 = ForAll(x) (Exists(y)(failed(y, x)))
    Assert.assertEquals("∀x Эy failed(y, x)", s2.toString)

    val parent = Predicate("parent")
    val child = Predicate("child")
    val s3 = ForAll(x)(ForAll(y)(parent(x, y) <-> child(y, x)))
    Assert.assertEquals("∀x ∀y parent(x, y) <-> child(y, x)", s3.toString)
  }
}
