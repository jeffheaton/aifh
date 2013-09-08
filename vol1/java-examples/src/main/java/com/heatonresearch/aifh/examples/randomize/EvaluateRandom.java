package com.heatonresearch.aifh.examples.randomize;

import com.heatonresearch.aifh.randomize.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Graphically evaluate various random number generators.
 */
public class EvaluateRandom extends JFrame implements ActionListener, Runnable {
    /**
     * The histogram pannel.
     */
    private HistoPanel histogram = new HistoPanel();

    /**
     * The start button.
     */
    private final JButton buttonStart;

    /**
     * The stop button.
     */
    private final JButton buttonStop;

    /**
     * Have we been requested to stop.
     */
    private boolean requestStop;

    /**
     * Uniform or normal.
     */
    private final JComboBox comboNormal;

    /**
     * The method to use.
     */
    private final JComboBox comboGenerator;

    /**
     * Create the window.
     */
    public EvaluateRandom() {
        String[] distributions = {"Uniform", "Normal"};
        String[] generators = {"Java", "LCG", "Secure", "Multiply With Carry (MWC)", "Mersenne Twister"};

        setSize(640, 480);
        Container content = this.getContentPane();
        content.setLayout(new BorderLayout());
        content.add(this.histogram = new HistoPanel(), BorderLayout.CENTER);

        JPanel controlPanel = new JPanel();
        controlPanel.setLayout(new GridLayout(3, 2));
        content.add(controlPanel, BorderLayout.SOUTH);

        this.buttonStart = new JButton("Start");
        this.buttonStop = new JButton("Stop");

        this.buttonStart.addActionListener(this);
        this.buttonStop.addActionListener(this);

        this.buttonStart.setEnabled(true);
        this.buttonStop.setEnabled(false);

        controlPanel.add(new JLabel("Random Generator"));
        controlPanel.add(this.comboGenerator = new JComboBox(generators));
        controlPanel.add(new JLabel("Normal Distribution"));
        controlPanel.add(this.comboNormal = new JComboBox(distributions));
        controlPanel.add(this.buttonStart);
        controlPanel.add(this.buttonStop);

        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        EvaluateRandom frame = new EvaluateRandom();
        frame.setVisible(true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed(final ActionEvent actionEvent) {
        if (actionEvent.getSource() == this.buttonStart) {
            this.buttonStart.setEnabled(false);
            this.buttonStop.setEnabled(false);
            Thread t = new Thread(this);
            t.start();
        } else {
            this.buttonStart.setEnabled(false);
            this.buttonStop.setEnabled(false);
            this.requestStop = true;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {
        this.requestStop = false;
        this.buttonStart.setEnabled(false);
        this.buttonStop.setEnabled(true);

        GenerateRandom rnd;

        switch (this.comboGenerator.getSelectedIndex()) {
            case 0:
                rnd = new BasicGenerateRandom();
                break;
            case 1:
                rnd = new LinearCongruentialRandom();
                break;
            case 2:
                rnd = new SecureGenerateRandom();
                break;
            case 3:
                rnd = new MultiplyWithCarryGenerateRandom();
                break;
            case 4:
                rnd = new MersenneTwisterGenerateRandom();
                break;
            default:
                rnd = new BasicGenerateRandom();
        }

        boolean uniform = this.comboNormal.getSelectedIndex() == 0;
        this.histogram.setUniformMode(uniform);
        this.histogram.reset();

        if (uniform) {
            while (!this.requestStop) {
                this.histogram.reportNumber(rnd.nextDouble());
            }
        } else {
            while (!this.requestStop) {
                this.histogram.reportNumber(rnd.nextGaussian());
            }
        }


        this.buttonStart.setEnabled(true);
        this.buttonStop.setEnabled(false);

        //To change body of implemented methods use File | Settings | File Templates.
    }
}
