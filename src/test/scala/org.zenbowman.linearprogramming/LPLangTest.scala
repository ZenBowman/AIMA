package org.zenbowman.linearprogramming

import junit.framework.TestCase

object Problem1 extends LPLang {
    6(x) + 5(y) maximize()
    1(x) + 1(y) <= 5
    3(x) + 2(y) <= 12
}

object Problem2 extends LPLang {
    1(x) + 1(y) maximize()
    1(x) + 1(y) <= 10
    2(x) + 3(y) <= 15
}

object Problem3 extends LPLang {
  300(x) + 36(y) + 90(z) maximize()
  6000(x) + 1200(y) + 1000(z) <= 12
  6000(x) + 600(y) + 3000(z) <= 15
}

class LPLangTest extends TestCase("LPLang") {

  def testSimple() {
    Problem1.solve
    Problem2.solve
    Problem3.solve
    println("Done!")
  }
}
