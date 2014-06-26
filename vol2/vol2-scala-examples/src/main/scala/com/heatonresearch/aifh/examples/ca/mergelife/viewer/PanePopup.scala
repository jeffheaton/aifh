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
import java.awt.event.ActionEvent
import java.awt.event.ActionListener

class PanePopup(val owner: DisplayPanel, val row: Int, val col: Int) extends JPopupMenu {
  val saveUniverse: JMenuItem = new JMenuItem("Save Physics")
  add(saveUniverse)
  val loadUniverse: JMenuItem = new JMenuItem("Load Physics")
  add(loadUniverse)
  addSeparator()
  val killReset: JMenuItem = new JMenuItem("Kill Universe")
  add(killReset)
  val bigBang: JMenuItem = new JMenuItem("Big Bang")
  add(bigBang)
  addSeparator()
  val mutateAcross: JMenuItem = new JMenuItem("Mutate Across")
  add(mutateAcross)
  val mutateSingle: JMenuItem = new JMenuItem("Mutate Single")
  add(mutateSingle)
  val crossover: JMenuItem = new JMenuItem("Crossover")
  add(crossover)
  addSeparator()
  val copyPane: JMenuItem = new JMenuItem("Copy Pane")
  add(copyPane)
  val runSingular: JMenuItem = new JMenuItem("Run Singular")
  add(runSingular)
  killReset.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.reset(PanePopup.this.row, PanePopup.this.col)
    }
  })
  bigBang.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.randomize(PanePopup.this.row, PanePopup.this.col)
    }
  })
  saveUniverse.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.save(PanePopup.this.row, PanePopup.this.col)
    }
  })
  loadUniverse.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.load(PanePopup.this.row, PanePopup.this.col)
    }
  })
  mutateAcross.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.mutateAcross(PanePopup.this.row, PanePopup.this.col)
    }
  })
  crossover.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.crossover(PanePopup.this.row, PanePopup.this.col)
    }
  })
  mutateSingle.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.mutateSingle(PanePopup.this.row, PanePopup.this.col)
    }
  })
  copyPane.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.copyPane(PanePopup.this.row, PanePopup.this.col)
    }
  })
  runSingular.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      PanePopup.this.owner.runSingular(PanePopup.this.row, PanePopup.this.col)
    }
  })
}