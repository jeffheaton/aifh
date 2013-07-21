package com.heatonresearch.aifh.randomize.example;

import com.heatonresearch.aifh.randomize.BasicGenerateRandom;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/21/13
 * Time: 2:00 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateRandom extends JFrame implements ActionListener, Runnable {

    private HistoPanel histogram = new HistoPanel();
    private JButton buttonStart;
    private JButton buttonStop;
    private boolean requestStop;

    public EvaluateRandom() {
        setSize(640, 480);
        Container content = this.getContentPane();
        content.setLayout(new BorderLayout());
        content.add(this.histogram = new HistoPanel(), BorderLayout.CENTER);

        JPanel controlPanel = new JPanel();
        controlPanel.setLayout(new GridLayout(4, 2));
        content.add(controlPanel, BorderLayout.SOUTH);

        this.buttonStart = new JButton("Start");
        this.buttonStop = new JButton("Stop");

        this.buttonStart.addActionListener(this);
        this.buttonStop.addActionListener(this);

        this.buttonStart.setEnabled(true);
        this.buttonStop.setEnabled(false);

        controlPanel.add(new JLabel("1"));
        controlPanel.add(new JLabel("2"));
        controlPanel.add(new JLabel("3"));
        controlPanel.add(new JLabel("4"));
        controlPanel.add(new JLabel("5"));
        controlPanel.add(new JLabel("6"));
        controlPanel.add(this.buttonStart);
        controlPanel.add(this.buttonStop);

        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    public static void main(String[] args) {
        EvaluateRandom frame = new EvaluateRandom();
        frame.setVisible(true);
    }

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

    @Override
    public void run() {
        this.requestStop = false;
        this.buttonStart.setEnabled(false);
        this.buttonStop.setEnabled(true);

        GenerateRandom rnd = new BasicGenerateRandom();

        while (!this.requestStop) {
            this.histogram.reportNumber(rnd.nextDouble());
        }

        this.buttonStart.setEnabled(true);
        this.buttonStop.setEnabled(false);

        //To change body of implemented methods use File | Settings | File Templates.
    }
}
