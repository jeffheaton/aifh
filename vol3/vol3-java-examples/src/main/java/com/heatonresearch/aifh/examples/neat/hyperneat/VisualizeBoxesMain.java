/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
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
package com.heatonresearch.aifh.examples.neat.hyperneat;

import org.encog.ml.ea.train.EvolutionaryAlgorithm;
import org.encog.neural.hyperneat.substrate.Substrate;
import org.encog.neural.hyperneat.substrate.SubstrateFactory;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;
import org.encog.neural.neat.training.species.OriginalNEATSpeciation;
import org.encog.util.Format;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * This program demonstrates HyperNEAT.
 *
 * The objective is to distinguish a large object from a small object in a two-
 * dimensional visual field. Because the same principle determines the
 * difference between small and large objects regardless of their location in
 * the retina, this task is well suited to testing the ability of HyperNEAT to
 * discover and exploit regularities.
 *
 * This program will display two rectangles, one large, and one small. The
 * program seeks to place the red position indicator in the middle of the larger
 * rectangle. The program trains and attempts to gain the maximum score of 110.
 * Once training is complete, you can run multiple test cases and see the
 * program attempt to find the center.
 *
 * One unique feature of HyperNEAT is that the resolution can be adjusted after
 * training has occured. This allows you to efficiently train on a small data
 * set and run with a much larger.
 *
 */
public class VisualizeBoxesMain extends JFrame implements Runnable,
        ActionListener {
    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;
    private final JButton btnTraining;
    private final JButton btnExample;
    private boolean trainingUnderway;
    private final JLabel labelIterations;
    private final JLabel labelError;
    private final JLabel labelSpecies;
    private boolean requestStop;
    private NEATPopulation pop;
    private EvolutionaryAlgorithm train;

    public VisualizeBoxesMain() {

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setTitle("Visualize Boxes");
        setSize(400, 200);
        Container content = this.getContentPane();
        content.setLayout(new BorderLayout());
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(1, 2));
        buttonPanel.add(this.btnTraining = new JButton("Start Training"));
        buttonPanel.add(this.btnExample = new JButton("Run Example"));
        content.add(buttonPanel, BorderLayout.SOUTH);
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new GridLayout(4, 2));
        content.add(mainPanel, BorderLayout.NORTH);
        mainPanel.add(new JLabel("Target (best) Score:"));
        mainPanel.add(new JLabel("110"));
        mainPanel.add(new JLabel("Current Score:"));
        mainPanel.add(this.labelError = new JLabel("N/A"));
        mainPanel.add(new JLabel("Iteration Count:"));
        mainPanel.add(this.labelIterations = new JLabel("0"));
        mainPanel.add(new JLabel("Species Count:"));
        mainPanel.add(this.labelSpecies = new JLabel("0"));

        this.btnTraining.addActionListener(this);
        this.btnExample.addActionListener(this);
        this.btnExample.setEnabled(false);
    }

    public void resetTraining() {
        Substrate substrate = SubstrateFactory.factorSandwichSubstrate(11, 11);
        BoxesScore score = new BoxesScore(11);
        this.pop = new NEATPopulation(substrate, 500);
        this.pop.setActivationCycles(4);
        this.pop.reset();
        this.train = NEATUtil.constructNEATTrainer(this.pop, score);
        OriginalNEATSpeciation speciation = new OriginalNEATSpeciation();
        speciation.setCompatibilityThreshold(1);
        this.train.setSpeciation(speciation = new OriginalNEATSpeciation());
        // train.setThreadCount(1);
    }

    public static void main(String[] args) {
        VisualizeBoxesMain boxes = new VisualizeBoxesMain();
        boxes.setVisible(true);
    }

    @Override
    public void run() {

        if (this.pop == null) {
            this.btnTraining.setEnabled(false);
            resetTraining();
        }

        // update the GUI
        this.btnTraining.setText("Stop Training");
        this.btnTraining.setEnabled(true);
        this.btnExample.setEnabled(false);
        this.trainingUnderway = true;

        this.requestStop = false;
        while (!this.requestStop && this.train.getError() < 110) {
            this.train.iteration();
            this.labelError.setText(Format.formatDouble(this.train.getError(), 2));
            this.labelIterations.setText(Format.formatInteger(this.train
                    .getIteration()));
            this.labelSpecies.setText(Format.formatInteger(this.pop
                    .getSpecies().size()));
        }

        this.train.finishTraining();

        this.btnTraining.setText("Start Training");
        this.btnExample.setEnabled(true);
        this.trainingUnderway = false;
    }

    private void beginTraining() {
        Thread t = new Thread(this);
        t.start();
    }

    public void handleTraining() {
        if (this.trainingUnderway) {
            this.requestStop = true;
        } else {
            beginTraining();
        }
    }

    @Override
    public void actionPerformed(ActionEvent ev) {
        if (ev.getSource() == this.btnTraining) {
            handleTraining();
        }
        if (ev.getSource() == this.btnExample) {
            DisplayBoxes display = new DisplayBoxes(this.pop);
            display.setVisible(true);
        }
    }
}
