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
package com.heatonresearch.aifh.examples.ca.mergelife.viewer;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class PanePopup extends JPopupMenu {

    private final int col;
    private final DisplayPanel owner;
    private final int row;

    public PanePopup(final DisplayPanel theOwner, final int theRow,
                     final int theCol) {
        final JMenuItem saveUniverse;
        add(saveUniverse = new JMenuItem("Save Physics"));
        final JMenuItem loadUniverse;
        add(loadUniverse = new JMenuItem("Load Physics"));
        addSeparator();
        final JMenuItem killReset;
        add(killReset = new JMenuItem("Kill Universe"));
        final JMenuItem bigBang;
        add(bigBang = new JMenuItem("Big Bang"));
        addSeparator();
        final JMenuItem mutateAcross;
        add(mutateAcross = new JMenuItem("Mutate Across"));
        final JMenuItem mutateSingle;
        add(mutateSingle = new JMenuItem("Mutate Single"));
        final JMenuItem crossover;
        add(crossover = new JMenuItem("Crossover"));
        addSeparator();
        final JMenuItem copyPane;
        add(copyPane = new JMenuItem("Copy Pane"));
        final JMenuItem runSingular;
        add(runSingular = new JMenuItem("Run Singular"));
        this.row = theRow;
        this.col = theCol;
        this.owner = theOwner;

        killReset.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.reset(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        bigBang.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.randomize(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        saveUniverse.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.save(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        loadUniverse.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.load(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        mutateAcross.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.mutateAcross(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        crossover.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.crossover(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        mutateSingle.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.mutateSingle(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        copyPane.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.copyPane(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
        runSingular.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                PanePopup.this.owner.runSingular(PanePopup.this.row,
                        PanePopup.this.col);
            }
        });
    }

}
