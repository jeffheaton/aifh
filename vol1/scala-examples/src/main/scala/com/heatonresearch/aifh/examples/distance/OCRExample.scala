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
package com.heatonresearch.aifh.examples.distance

import com.heatonresearch.aifh.distance.{CalculateDistance, EuclideanDistance}
import javax.swing._
import java.awt._
import java.awt.event.{ActionListener, ActionEvent}
import java.io._
import javax.swing.event.{ListSelectionListener, ListSelectionEvent}

/**
 * OCR: Main form that allows the user to interact with the OCR application.
 */
object OCRExample extends App {
  /**
   * The downsample width for the application.
   */
  val DOWNSAMPLE_WIDTH: Int = 5
  /**
   * The down sample height for the application.
   */
  val DOWNSAMPLE_HEIGHT: Int = 7

  (new OCRExample).setVisible(true)
}

@SerialVersionUID(-6779380961875907013L)
class OCRExample extends JFrame("OCR") {

  /**
   * The entry component for the user to draw into.
   */
  private val entry = new Entry
  /**
   * The down sample component to display the drawing downsampled.
   */
  private val sample = new Sample(OCRExample.DOWNSAMPLE_WIDTH, OCRExample.DOWNSAMPLE_HEIGHT)
  /**
   * The letters that have been defined.
   */
  private val letterListModel: DefaultListModel[SampleData] = new DefaultListModel
  /**
   * THe downsample button.
   */
  private val downSample = new JButton("D Sample")
  /**
   * The add button.
   */
  private val add = new JButton("Add")
  /**
   * The clear button
   */
  private val clear = new JButton("Clear")
  /**
   * The recognize button
   */
  private val recognize = new JButton("Recognize")
  /**
   * The letters list box
   */
  private val letters = new JList[SampleData]
  /**
   * The delete button
   */
  private val del = new JButton("Delete")
  /**
   * The load button
   */
  private val load = new JButton("Load")
  /**
   * The save button
   */
  private val save = new JButton("Save")
  private val JLabel3 = new JLabel
  private val JLabel8 = new JLabel
  private val JLabel5 = new JLabel("Draw Letters Here")
  private val distanceCalc: CalculateDistance = new EuclideanDistance

  getContentPane.setLayout(null)
  entry.setLocation(168, 25)
  entry.setSize(200, 128)
  getContentPane.add(entry)
  sample.setLocation(307, 210)
  sample.setSize(65, 70)
  entry.sample = sample
  getContentPane.add(sample)
  getContentPane.setLayout(null)
  setSize(405, 382)
  setVisible(false)
  val JLabel1: JLabel = new JLabel("Letters Known")
  getContentPane.add(JLabel1)
  JLabel1.setBounds(12, 12, 100, 12)
  val JLabel2: JLabel = new JLabel
  JLabel2.setBounds(12, 264, 72, 24)
  downSample.setActionCommand("Down Sample")
  getContentPane.add(downSample)
  downSample.setBounds(252, 180, 120, 24)
  add.setActionCommand("Add")
  getContentPane.add(add)
  add.setBounds(168, 156, 84, 24)
  clear.setActionCommand("Clear")
  getContentPane.add(clear)
  clear.setBounds(168, 180, 84, 24)
  recognize.setActionCommand("Recognize")
  getContentPane.add(recognize)
  recognize.setBounds(252, 156, 120, 24)
  val JScrollPane1 = new JScrollPane
  JScrollPane1.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
  JScrollPane1.setOpaque(true)
  getContentPane.add(JScrollPane1)
  JScrollPane1.setBounds(12, 24, 144, 132)
  JScrollPane1.getViewport.add(letters)
  letters.setBounds(0, 0, 126, 129)
  del.setActionCommand("Delete")
  getContentPane.add(del)
  del.setBounds(12, 156, 144, 24)
  load.setActionCommand("Load")
  getContentPane.add(load)
  load.setBounds(12, 180, 75, 24)
  save.setActionCommand("Save")
  getContentPane.add(save)
  save.setBounds(84, 180, 72, 24)
  JLabel3.setBounds(12, 288, 72, 24)
  JLabel8.setHorizontalTextPosition(SwingConstants.CENTER)
  JLabel8.setHorizontalAlignment(SwingConstants.CENTER)
  JLabel8.setFont(new Font("Dialog", Font.BOLD, 14))
  JLabel8.setBounds(12, 240, 120, 24)

  getContentPane.add(JLabel5)
  JLabel5.setBounds(204, 12, 144, 12)

  val lSymAction = new SymAction
  downSample.addActionListener(lSymAction)
  clear.addActionListener(lSymAction)
  add.addActionListener(lSymAction)
  del.addActionListener(lSymAction)

  val lSymListSelection = new SymListSelection
  letters.addListSelectionListener(lSymListSelection)
  load.addActionListener(lSymAction)
  save.addActionListener(lSymAction)
  recognize.addActionListener(lSymAction)
  letters.setModel(letterListModel)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)


  /**
   * Called to add the current image to the training set
   */
  private[distance] def add_actionPerformed() {
    val letter = JOptionPane.showInputDialog("Please enter a letter you would like to assign this sample to.")
    if (letter == null)
      return

    if (letter.length > 1) {
      JOptionPane.showMessageDialog(this, "Please enter only a single letter.", "Error", JOptionPane.ERROR_MESSAGE)
      return
    }
    entry.downSample()

    val sampleData: SampleData = sample.data.cloneInstance(letter.charAt(0))
    for(i <- 0 until letterListModel.size) {
      val str = "" + letterListModel.getElementAt(i).letter
      if (str == letter) {
        JOptionPane.showMessageDialog(this, "That letter is already defined, delete it first!", "Error", JOptionPane.ERROR_MESSAGE)
        return
      }
      val l = "" + sampleData.letter
      if (str.compareTo(l) > 0) {
        letterListModel.add(i, sampleData)
        return
      }
    }
    letterListModel.add(letterListModel.size, sampleData)
    letters.setSelectedIndex(letterListModel.size-1)
    entry.clear()
    sample.repaint()
  }

  /**
   * Called to clear the image.
   */
  private def clear_actionPerformed() {
    entry.clear()
    sample.data.clear()
    sample.repaint()
  }

  /**
   * Called when the del button is pressed.
   */
  private[distance] def del_actionPerformed() {
    val i = letters.getSelectedIndex
    if (i == -1) {
      JOptionPane.showMessageDialog(this, "Please select a letter to delete.", "Error", JOptionPane.ERROR_MESSAGE)
      return
    }
    letterListModel.remove(i)
  }

  /**
   * Called to downsample the image.
   */
  private[distance] def downSample_actionPerformed() {
    entry.downSample()
  }

  /**
   * Called when a letter is selected from the list box.
   */
  private def letters_valueChanged() {
    if (letters.getSelectedIndex != -1) {
      val selected = letterListModel.getElementAt(letters.getSelectedIndex)
      sample.data = selected.cloneInstance()
      sample.repaint()
      entry.clear()
    }
  }

  /**
   * Called when the load button is pressed.
   */
  private def load_actionPerformed() {
    try {
      val f = new FileReader(new File("./sample.dat"))
      val r = new BufferedReader(f)

      var line: String = null
      var i: Int = 0
      letterListModel.clear()
      while ({ line = r.readLine ; line } != null) {
        val ds: SampleData = new SampleData(line.charAt(0), OCRExample.DOWNSAMPLE_WIDTH, OCRExample.DOWNSAMPLE_HEIGHT)
        letterListModel.add(i, ds)
        i += 1

        var idx: Int = 2
        for(y <- 0 until ds.height;
            x <- 0 until ds.width) {
          ds.setData(x, y, line.charAt(idx) == '1')
          idx += 1
        }
      }
      r.close()
      f.close()
      clear_actionPerformed()
      JOptionPane.showMessageDialog(this, "Loaded from 'sample.dat'.", "Training", JOptionPane.PLAIN_MESSAGE)
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        JOptionPane.showMessageDialog(this, "Error: " + e, "Training", JOptionPane.ERROR_MESSAGE)
    }
  }

  /**
   * Called when the recognize button is pressed.
   */
  private def recognize_actionPerformed() {
    entry.downSample()
    var bestPosition: Double = Double.PositiveInfinity
    var letter: String = "?"
    val letterToRecognize: Vector[Double] = sample.data.getPosition
    for(i <- 0 until letterListModel.size) {
      val ds = letterListModel.getElementAt(i)
      val dist: Double = distanceCalc.calculate(letterToRecognize, ds.getPosition)
      if (dist < bestPosition) {
        bestPosition = dist
        letter = "" + ds.letter
      }
    }
    JOptionPane.showMessageDialog(this, letter, "That Letter Is", JOptionPane.PLAIN_MESSAGE)
    clear_actionPerformed()
  }

  /**
   * Called when the save button is clicked.
   */
  private def save_actionPerformed() {
    try {
      val os: OutputStream = new FileOutputStream("./sample.dat", false)
      val ps: PrintStream = new PrintStream(os)
      for(i <- 0 until letterListModel.size) {
        val ds = letterListModel.elementAt(i)
        ps.print(ds.letter + ":")
        for(y <- 0 until ds.height;
            x <- 0 until ds.width) {
          ps.print(if (ds.getData(x, y)) "1" else "0")
        }
        ps.println()
      }

      ps.close()
      os.close()
      clear_actionPerformed()
      JOptionPane.showMessageDialog(this, "Saved to 'sample.dat'.", "Training", JOptionPane.PLAIN_MESSAGE)
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        JOptionPane.showMessageDialog(this, "Error: " + e, "Training", JOptionPane.ERROR_MESSAGE)
    }
  }

  private[distance] class SymAction extends ActionListener {
    def actionPerformed(event: ActionEvent) {
      event.getSource match {
        case `downSample` => downSample_actionPerformed()
        case `clear` =>      clear_actionPerformed()
        case `add` =>        add_actionPerformed()
        case `del` =>        del_actionPerformed()
        case `save` =>       save_actionPerformed()
        case `load` =>       load_actionPerformed()
        case `recognize` =>  recognize_actionPerformed()
        case _ =>
      }
    }
  }

  private[distance] class SymListSelection extends ListSelectionListener {
    override def valueChanged(event: ListSelectionEvent) {
      if (event.getSource == letters)
        letters_valueChanged()
    }
  }
}
