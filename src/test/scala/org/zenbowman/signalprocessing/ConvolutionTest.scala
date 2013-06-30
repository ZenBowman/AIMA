package org.zenbowman.signalprocessing

import junit.framework.TestCase

class ConvolutionTest extends TestCase {
  def test_2_8() {
    val signal = new Signal(List(1,2,3,-1))
    val filter = new Filter(List(1,-1,2))
    val result = filter.convolve(signal)
    Console.println(result)
  }
}
