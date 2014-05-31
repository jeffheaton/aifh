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
package com.heatonresearch.aifh.examples.ca

import com.heatonresearch.aifh.examples.util.WorldPanel
import javax.swing._
import java.awt._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.WindowEvent
import java.awt.event.WindowListener

/**
 * This example implements an elementary cellular automation.
 * <p/>
 * References:
 * http://mathworld.wolfram.com/ElementaryCellularAutomaton.html
 */
object ElementaryCA {
  /**
   * The main entry point.
   *
   * @param args The arguments.
   */
  def main(args: Array[String]) {
    try {
      val f: JFrame = new ElementaryCA
      f.setVisible(true)
    }
    catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }

  /**
   * The number of rows.
   */
  val ROWS: Int = 200
  /**
   * The number of columns.
   */
  val COLS: Int = 200
}

class ElementaryCA extends JFrame with ActionListener with WindowListener {
  import ElementaryCA._
  /** The generate button. */
  private val generateButton = new JButton("Generate")
  /** The world area. */
  private val worldArea: WorldPanel = new WorldPanel(ROWS, COLS, false)
  /** Allow rule # to be entered. */
  private val ruleInput: TextField = new TextField("30",5)

  {
    setSize(500, 500)
    setTitle("Elementary Cellular Automation")
    val c: Container = getContentPane
    c.setLayout(new BorderLayout)
    val buttonPanel: JPanel = new JPanel
    buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT))
    c.add(buttonPanel, BorderLayout.NORTH)
    buttonPanel.add(new Label("Rule (0-255):"))
    buttonPanel.add(ruleInput)
    buttonPanel.add(generateButton)
    val scroll: JScrollPane = new JScrollPane(this.worldArea)
    c.add(scroll, BorderLayout.CENTER)
    generateButton.addActionListener(this)
    this.addWindowListener(this)
  }

  override def actionPerformed(ev: ActionEvent) {
    if (ev.getSource eq generateButton) {
      performGenerate()
      repaint()
    }
  }

  override def windowActivated(arg0: WindowEvent) {}

  override def windowClosed(arg0: WindowEvent) {}

  override def windowClosing(arg0: WindowEvent) {
    System.exit(0)
  }

  override def windowDeactivated(arg0: WindowEvent) {}

  override def windowDeiconified(arg0: WindowEvent) {}

  override def windowIconified(arg0: WindowEvent) {}

  override def windowOpened(arg0: WindowEvent) {
    performGenerate()
  }

  /**
   * Generate the CA.
   */
  def performGenerate() {
    var invalid: Boolean = false
    val output: Array[Boolean] = new Array[Boolean](8)
    val center: Int = this.worldArea.getCols / 2
    this.worldArea.getPrimaryGrid(0)(center) = true
    val grid: Array[Array[Boolean]] = this.worldArea.getPrimaryGrid
    var rule: Int = 30
    try {
      rule = Integer.parseInt(this.ruleInput.getText)
      if (rule < 0 || rule > 255) {
        invalid = true
      }
    }
    catch {
      case ex: NumberFormatException =>
        invalid = true
    }
    if (invalid) {
      JOptionPane.showMessageDialog(null, "Invalid rule number, must be between 0 and 255.")
      return
    }
    var cx: Int = 1
    var idx: Int = 7
    while (idx > 0) {
      output(idx) = (rule & cx) != 0
      idx -= 1
      cx *= 2
    }
    for(row <- 1 until this.worldArea.getRows) {
      val prevRow: Int = row - 1
      for(i <- 0 until this.worldArea.getCols - 2) {
        var result: Boolean = false
        val a: Boolean = grid(prevRow)(i)
        val b: Boolean = grid(prevRow)(i + 1)
        val c: Boolean = grid(prevRow)(i + 2)
        if (a && b && c) {
          result = output(0)
        }
        else if (a && b && !c) {
          result = output(1)
        }
        else if (a && !b && c) {
          result = output(2)
        }
        else if (a && !b && !c) {
          result = output(3)
        }
        else if (!a && b && c) {
          result = output(4)
        }
        else if (!a && b && !c) {
          result = output(5)
        }
        else if (!a && !b && c) {
          result = output(6)
        }
        else if (!a && !b && !c) {
          result = output(7)
        }
        grid(row)(i + 1) = result
      }
    }
    this.repaint()
  }
}