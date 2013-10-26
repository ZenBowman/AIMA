package org.zenbowman.shapiro.computer.vision

import junit.framework.TestCase

class TestSampleBinarizer extends TestCase {
  def testBinarizer() {
    val original = List(
      List(1, 0, 0, 0, 0, 0, 0, 0),
      List(1, 0, 0, 0, 0, 0, 0, 0),
      List(0, 0, 0, 0, 0, 0, 0, 1),
      List(0, 2, 2, 0, 1, 0, 1, 2)
    )

    val (f, s) = SampleBinarizer.binarizeSamples(original)
    for (fs <- f) {
      println(fs)
    }
    println(s)
  }
}
