/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.examples.ca.mergelife.viewer

import javax.swing._
import java.awt._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.io._

/**
 * This example shows how to use a Human Based Genetic Algorithm to evolve a set of physical constants to create
 * an interesting cellular automation. You are shown a multiverse, with many different running universes. You can
 * choose universes that look interesting, and experiment with crossover and mutation to create more interesting
 * universes.
 * <p/>
 * For complete instructions, refer to the following article.
 * <p/>
 * http://www.codeproject.com/Articles/730362/Using-an-evolutionary-algorithm-to-create-a-cellul
 * <p/>
 * Other references.
 * <p/>
 * http://en.wikipedia.org/wiki/Human-based_genetic_algorithm
 * http://en.wikipedia.org/wiki/Cellular_automaton
 */
object MultiverseViewer {
  /**
   * @return the config
   */
  def getConfig: ConfigData = MultiverseViewer.config

  /**
   * Load the configuration data.
   */
  private def loadConfig() {
    try {
      val home = new File(System.getProperty("user.home"))
      val configFile = new File(home, MultiverseViewer.CONFIG_NAME)
      val fileIn = new FileInputStream(configFile)
      val in = new ObjectInputStream(fileIn)
      MultiverseViewer.config = in.readObject.asInstanceOf[ConfigData]
      in.close()
      fileIn.close()
    }
    catch {
      case ex: IOException =>
        MultiverseViewer.config = new ConfigData
      case ex: ClassNotFoundException =>
        ex.printStackTrace()
    }
  }

  /**
   * Main entry point.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) {
    try {
      val f: JFrame = new MultiverseViewer
      f.setVisible(true)
    }
    catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }

  /**
   * Save the configuration file.
   */
  def saveConfig() {
    try {
      val home: File = new File(System.getProperty("user.home"))
      val configFile: File = new File(home, MultiverseViewer.CONFIG_NAME)
      val fileOut: FileOutputStream = new FileOutputStream(configFile)
      val out: ObjectOutputStream = new ObjectOutputStream(fileOut)
      out.writeObject(MultiverseViewer.config)
      out.close()
      fileOut.close()
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }

  /**
   * @param config the config to set
   */
  def setConfig(config: ConfigData) {
    MultiverseViewer.config = config
  }

  /**
   * Configuration data.
   */
  private var config: ConfigData = new ConfigData
  /**
   * The file that holds configuration data.
   */
  val CONFIG_NAME: String = ".multiverse_viewer.ser"
}

class MultiverseViewer extends JFrame with ActionListener with WindowListener with Runnable {


  MultiverseViewer.loadConfig()
  setSize(1024, 768)
  setTitle("Merge Life - Multiverse!")
  val rows = MultiverseViewer.getConfig.universePaneRows
  val cols = MultiverseViewer.getConfig.universePaneColumns
  val c = getContentPane
  c.setLayout(new BorderLayout)
  val buttonPanel = new JPanel

  /**
   * Allow universes to be auto-killed if they stop moving.
   */
  private val autoKill = new JCheckBox("Auto Kill")
  /**
   * Open the configuration dialog.
   */
  private val configButton = new JButton("Config...")
  /**
   * Deselect any universes.
   */
  private val deselectButton = new JButton("Deselect")
  /**
   * Simulate a single frame.
   */
  private val goButton = new JButton("Single Step")
  /**
   * The multiverse view panel.
   */
  private val outputPanel = new DisplayPanel(this, rows, cols)
  /**
   * Reset all universes.
   */
  private val resetButton = new JButton("Reset")
  /**
   * Are the universes running?
   */
  private var running = false
  /**
   * Start all universes.
   */
  private val startButton = new JButton("Start")
  /**
   * Stop all universes.
   */
  private val stopButton = new JButton("Stop")
  /**
   * Has a stop been requested?
   */
  private var stopRequest = false

  c.add(buttonPanel, BorderLayout.NORTH)
  buttonPanel.add(configButton)
  buttonPanel.add(goButton)
  buttonPanel.add(startButton)
  buttonPanel.add(stopButton)
  buttonPanel.add(resetButton)
  buttonPanel.add(autoKill)
  buttonPanel.add(deselectButton)
  c.add(outputPanel, BorderLayout.CENTER)
  this.goButton.addActionListener(this)
  this.startButton.addActionListener(this)
  this.stopButton.addActionListener(this)
  this.resetButton.addActionListener(this)
  this.configButton.addActionListener(this)
  this.autoKill.addActionListener(this)
  this.deselectButton.addActionListener(this)
  this.outputPanel.setAutoKill(autoKill = true)
  this.autoKill.setSelected(true)
  this.deselectButton.setEnabled(false)
  addWindowListener(this)

  // ---- end of construction ----

  override def actionPerformed(ev: ActionEvent) {
    if (ev.getSource eq this.goButton) {
      this.outputPanel.update()
    }
    else if (ev.getSource eq this.startButton) {
      performStart()
    }
    else if (ev.getSource eq this.stopButton) {
      performStop()
    }
    else if (ev.getSource eq this.resetButton) {
      performReset()
    }
    else if (ev.getSource eq this.configButton) {
      performConfig()
    }
    else if (ev.getSource eq this.autoKill) {
      performAutoKill()
    }
    else if (ev.getSource eq this.deselectButton) {
      performDeselect()
    }
  }

  /**
   * Allow the deselect button.
   */
  def enableDeselect() {
    this.deselectButton.setEnabled(true)
  }

  /**
   * Set auto kill mode.
   */
  def performAutoKill() {
    this.outputPanel.setAutoKill(this.autoKill.isSelected)
  }

  /**
   * Open the config dialog.
   */
  private def performConfig() {
    val dialog: ConfigDialog = new ConfigDialog
    dialog.setVisible(true)
  }

  /**
   * Deselect any selected universe.
   */
  def performDeselect() {
    this.outputPanel.deselect()
    this.deselectButton.setEnabled(false)
  }

  /**
   * Perform a reset of the universe.
   */
  def performReset() {
    this.outputPanel.resetAll()
  }

  /**
   * Start the universes.
   */
  def performStart() {
    if (!this.running) {
      val t: Thread = new Thread(this)
      t.start()
    }
  }

  /**
   * Stop the universes.
   */
  def performStop() {
    if (this.running) {
      this.stopRequest = true
    }
  }

  override def run() {
    this.running = true
    while (!this.stopRequest) {
      this.outputPanel.update()
    }
    this.stopRequest = false
    this.running = false
  }

  override def windowActivated(arg0: WindowEvent) {}

  override def windowClosed(arg0: WindowEvent) {}

  override def windowClosing(arg0: WindowEvent) {
    MultiverseViewer.saveConfig()
    System.exit(0)
  }

  override def windowDeactivated(arg0: WindowEvent) {}

  override def windowDeiconified(arg0: WindowEvent) {}

  override def windowIconified(arg0: WindowEvent) {}

  override def windowOpened(arg0: WindowEvent) {}
}