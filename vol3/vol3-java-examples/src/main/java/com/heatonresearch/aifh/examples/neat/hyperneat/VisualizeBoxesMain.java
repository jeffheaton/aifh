package com.heatonresearch.aifh.examples.neat.hyperneat;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.encog.ml.ea.train.EvolutionaryAlgorithm;
import org.encog.neural.hyperneat.substrate.Substrate;
import org.encog.neural.hyperneat.substrate.SubstrateFactory;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;
import org.encog.neural.neat.training.species.OriginalNEATSpeciation;
import org.encog.util.Format;

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
    private JButton btnTraining;
    private JButton btnExample;
    private boolean trainingUnderway;
    private JLabel labelIterations;
    private JLabel labelError;
    private JLabel labelSpecies;
    private boolean requestStop = false;
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
        pop = new NEATPopulation(substrate, 500);
        pop.setActivationCycles(4);
        pop.reset();
        train = NEATUtil.constructNEATTrainer(pop, score);
        OriginalNEATSpeciation speciation = new OriginalNEATSpeciation();
        speciation.setCompatibilityThreshold(1);
        train.setSpeciation(speciation = new OriginalNEATSpeciation());
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
            this.labelError.setText(Format.formatDouble(train.getError(), 2));
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
            DisplayBoxes display = new DisplayBoxes(pop);
            display.setVisible(true);
        }
    }
}
