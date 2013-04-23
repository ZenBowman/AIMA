package org.zenbowman.linearprogramming

import junit.framework.{Assert, TestCase}

object Problem1 extends LPLang {

    6(x) + 5(y) maximize()
    1(x) + 1(y) <= 5
    3(x) + 2(y) <= 12
    //solve
    //println("foo")
}

class LPLangTest extends TestCase("LPLang") {

  def testSimple() {
    Problem1.solve
    println("Done!")
  }
}
