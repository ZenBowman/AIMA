package org.zenbowman.sicp

object SicpMain {
  def main(args: Array[String]) {
    args(0) match {
      case "2" =>
        Chapter2(args(1))
      case _ =>
        println("Unknown chapter")
    }
  }
}
