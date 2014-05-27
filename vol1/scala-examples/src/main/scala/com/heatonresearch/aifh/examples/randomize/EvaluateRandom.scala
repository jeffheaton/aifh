/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Scala Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.examples.randomize

import com.heatonresearch.aifh.randomize._
import javax.swing._
import java.awt._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener

/**
 * Graphically evaluate various random number generators.
 */
object EvaluateRandom extends App {
  (new EvaluateRandom).setVisible(true)
}

class EvaluateRandom extends JFrame with ActionListener with Runnable {

  /**
   * The histogram panel.
   */
  private val histogram = new HistoPanel
  /**
   * The start button.
   */
  private val buttonStart = new JButton("Start")
  /**
   * The stop button.
   */
  private val buttonStop = new JButton("Stop")
  /**
   * Have we been requested to stop.
   */
  @volatile
  private var requestStop = false


  val distributions: Array[Object] = Array("Uniform", "Normal")
  val generators: Array[Object] = Array("Java", "LCG", "Secure", "Multiply With Carry (MWC)", "Mersenne Twister")
  setSize(640, 480)
  val content: Container = this.getContentPane
  content.setLayout(new BorderLayout)
  content.add(histogram, BorderLayout.CENTER)
  val controlPanel: JPanel = new JPanel
  controlPanel.setLayout(new GridLayout(3, 2))
  content.add(controlPanel, BorderLayout.SOUTH)
  buttonStart.addActionListener(this)
  buttonStop.addActionListener(this)
  buttonStart.setEnabled(true)
  buttonStop.setEnabled(false)
  controlPanel.add(new JLabel("Random Generator"))

  /**
   * The method to use.
   */
  private val comboGenerator = new JComboBox(generators)

  controlPanel.add(comboGenerator)
  controlPanel.add(new JLabel("Normal Distribution"))

  /**
   * Uniform or normal.
   */
  private val comboNormal = new JComboBox(distributions)

  controlPanel.add(comboNormal)
  controlPanel.add(buttonStart)
  controlPanel.add(buttonStop)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)


  def actionPerformed(actionEvent: ActionEvent) {
    if (actionEvent.getSource eq buttonStart) {
      buttonStart.setEnabled(false)
      buttonStop.setEnabled(false)
      val t = new Thread(this)
      t.start()
    }
    else {
      buttonStart.setEnabled(false)
      buttonStop.setEnabled(false)
      this.requestStop = true
    }
  }

  override def run() {
    requestStop = false
    buttonStart.setEnabled(false)
    buttonStop.setEnabled(true)
    val rnd: GenerateRandom = comboGenerator.getSelectedIndex match {
      case 0 => new BasicGenerateRandom()
      case 1 => new LinearCongruentialRandom()
      case 2 => new SecureGenerateRandom()
      case 3 => new MultiplyWithCarryGenerateRandom()
      case 4 => new MersenneTwisterGenerateRandom()
      case _ => new BasicGenerateRandom()
    }
    val uniform = this.comboNormal.getSelectedIndex == 0
    histogram.uniformMode = uniform
    histogram.reset()
    if (uniform) {
      while (!requestStop) {
        histogram.reportNumber(rnd.nextDouble())
      }
    }
    else {
      while (!requestStop)
        histogram.reportNumber(rnd.nextGaussian)
    }
    buttonStart.setEnabled(true)
    buttonStop.setEnabled(false)
  }
}
