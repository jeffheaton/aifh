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

class ConfigDialog extends JDialog with ActionListener {

  private val buttonCancel = new JButton("Cancel")
  private val buttonDefaults = new JButton("Defaults")
  private val buttonOK: JButton = new JButton("OK")
  private val textPaneHeight = new JTextField
  private val textPaneWidth = new JTextField
  private val textUniversePaneColumns = new JTextField
  private val textUniversePaneRows = new JTextField
  private val textZoom = new JTextField

  setTitle("Configuration")
  setSize(640, 480)
  val pannelButtons: JPanel = new JPanel
  setLayout(new BorderLayout)
  pannelButtons.setLayout(new FlowLayout)
  pannelButtons.add(buttonOK)
  pannelButtons.add(buttonDefaults)
  pannelButtons.add(buttonCancel)
  add(pannelButtons, BorderLayout.SOUTH)
  val panelContent: JPanel = new JPanel
  panelContent.setLayout(new GridLayout(5, 2))
  add(panelContent, BorderLayout.CENTER)
  panelContent.add(new JLabel("Universe Pane Height:"))
  panelContent.add(textPaneHeight)
  panelContent.add(new JLabel("Universe Pane Width:"))
  panelContent.add(textPaneWidth)
  panelContent.add(new JLabel("Universe Pane Rows"))
  panelContent.add(textUniversePaneRows)
  panelContent.add(new JLabel("Universe Pane Columns"))
  panelContent.add(textUniversePaneColumns)
  panelContent.add(new JLabel("Zoom:"))
  panelContent.add(textZoom)
  setFields()
  this.buttonOK.addActionListener(this)
  this.buttonCancel.addActionListener(this)
  this.buttonDefaults.addActionListener(this)

  def actionPerformed(e: ActionEvent) {
    if (e.getSource eq this.buttonOK) {
      MultiverseViewer.getConfig.paneHeight = textPaneHeight.getText.toInt
      MultiverseViewer.getConfig.paneWidth = textPaneWidth.getText.toInt
      MultiverseViewer.getConfig.universePaneRows = textUniversePaneRows.getText.toInt
      MultiverseViewer.getConfig.universePaneColumns = textUniversePaneColumns.getText.toInt
      MultiverseViewer.getConfig.setZoom(Integer.parseInt(this.textZoom.getText))
      MultiverseViewer.saveConfig()
      dispose()
    }
    else if (e.getSource eq this.buttonDefaults) {
      MultiverseViewer.setConfig(new ConfigData)
      setFields()
    }
    else if (e.getSource eq this.buttonCancel) {
      dispose()
    }
  }

  private def setFields() {
    this.textPaneHeight.setText(MultiverseViewer.getConfig.paneHeight.toString)
    this.textPaneWidth.setText(MultiverseViewer.getConfig.paneWidth.toString)
    this.textUniversePaneRows.setText(MultiverseViewer.getConfig.universePaneRows.toString)
    this.textUniversePaneColumns.setText(MultiverseViewer.getConfig.universePaneColumns.toString)
    this.textZoom.setText("" + MultiverseViewer.getConfig.getZoom)
  }
}