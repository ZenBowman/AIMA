package org.zenbowman.signalprocessing

import org.jfree.data.xy.{XYSeriesCollection, XYDataset, XYSeries}
import org.jfree.chart.{ChartPanel, ChartFactory}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.ui.ApplicationFrame

object SignalPlotter {
  val signalDataset = new XYSeriesCollection()

  def plot(name: String, signal: Signal) {
    plot(name, signal.values)
  }

  def show() {
    val chart = ChartFactory.createXYLineChart(
      "Signal plot", "n", "y[n]",
      signalDataset, PlotOrientation.VERTICAL,
      true, true, false)
    val chartPanel = new ChartPanel(chart)
    chartPanel.setPreferredSize(new java.awt.Dimension(500, 270))

    val app = new ApplicationFrame("Signal")
    app.setContentPane(chartPanel)
    app.pack()
    app.setVisible(true)
  }

  def plot(name: String, points: Seq[Float]) {
    val signalPlot = new XYSeries(name)
    for (p <- 0 until points.length) {
      signalPlot.add(p, points(p).toDouble)
    }
    signalDataset.addSeries(signalPlot)
  }
}
