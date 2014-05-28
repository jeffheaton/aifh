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
package com.heatonresearch.aifh.examples.ca;

import com.heatonresearch.aifh.examples.util.WorldPanel;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

/**
 * Conway's game of life.  A classic cellular automation.
 * <p/>
 * Rules:
 * 1. Any live cell with fewer than two live neighbors dies, as if caused by under-population.
 * 2. Any live cell with two or three live neighbors lives on to the next generation. (not needed)
 * 3. Any live cell with more than three live neighbors dies, as if by overcrowding.
 * 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
 * <p/>
 * References:
 * http://en.wikipedia.org/wiki/Conway's_Game_of_Life
 */
public class ConwayCA extends JFrame implements ActionListener, WindowListener, Runnable {
    /**
     * Perform an iteration.
     */
    private JButton iterationButton;

    /**
     * Start running.
     */
    private JButton startButton;

    /**
     * Stop running.
     */
    private JButton stopButton;

    /**
     * Reset the simulation.
     */
    private JButton resetButton;

    /**
     * Has a stop been requested.
     */
    private boolean requestStop;

    /**
     * The world.
     */
    private WorldPanel worldArea;

    /**
     * Used to locate the x coordinate of neighbors.
     */
    private static final int[] neighborsX = {0, 0, 1, -1, -1, 1, -1, 1};

    /**
     * Used to locate the y coordinate of neighbors.
     */
    private static final int[] neighborsY = {1, -1, 0, 0, -1, -1, 1, 1};

    /**
     * The number of rows.
     */
    public static final int ROWS = 75;

    /**
     * The number of columns.
     */
    public static final int COLS = 75;

    /**
     * The constructor.
     */
    public ConwayCA() {
        setSize(500, 500);
        setTitle("Conway's Game of Life");

        Container c = getContentPane();
        c.setLayout(new BorderLayout());
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        c.add(buttonPanel, BorderLayout.NORTH);
        buttonPanel.add(iterationButton = new JButton("Iteration"));
        buttonPanel.add(startButton = new JButton("Start"));
        buttonPanel.add(stopButton = new JButton("Stop"));
        buttonPanel.add(resetButton = new JButton("Reset"));

        this.worldArea = new WorldPanel(ROWS, COLS, true);
        /*
      Scroll bar.
     */
        final JScrollPane scroll = new JScrollPane(this.worldArea);
        c.add(scroll, BorderLayout.CENTER);
        iterationButton.addActionListener(this);
        startButton.addActionListener(this);
        stopButton.addActionListener(this);
        resetButton.addActionListener(this);

        this.addWindowListener(this);
        this.stopButton.setEnabled(false);

        performReset();
    }

    /**
     * Perform one iteration.
     */
    public void performIteration() {
        boolean[][] grid = this.worldArea.getPrimaryGrid();

        for (int row = 0; row < grid.length; row++) {
            for (int col = 0; col < grid[row].length; col++) {
                int total = 0;

                for (int i = 0; i < neighborsX.length; i++) {
                    int nCol = col + neighborsX[i];
                    int nRow = row + neighborsY[i];
                    if (nCol >= 0 && nCol < this.worldArea.getCols()) {
                        if (nRow >= 0 && nRow < this.worldArea.getRows()) {
                            if (grid[nRow][nCol]) {
                                total++;
                            }
                        }
                    }
                }


                boolean alive = grid[row][col];

                if (alive) {
                    // 1. Any live cell with fewer than two live neighbors dies, as if caused by under-population.
                    if (total < 2) {
                        alive = false;
                    }
                    // 2. Any live cell with two or three live neighbors lives on to the next generation. (not needed)
                    // 3. Any live cell with more than three live neighbors dies, as if by overcrowding.
                    if (alive && total > 3) {
                        alive = false;
                    }
                } else {
                    // 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
                    if (total == 3) {
                        alive = true;
                    }
                }

                this.worldArea.getBackupGrid()[row][col] = alive;
            }
        }

        this.worldArea.advanceBackupGrid();
    }

    /**
     * Start the simulation.
     */
    public void performStart() {
        this.iterationButton.setEnabled(false);
        this.stopButton.setEnabled(true);
        this.startButton.setEnabled(false);
        /*
      The background thread.
     */
        final Thread thread = new Thread(this);
        thread.start();
    }

    /**
     * Request stop.
     */
    public void performStop() {
        this.requestStop = true;
    }

    /**
     * Perform a reset.
     */
    public void performReset() {
        boolean[][] grid = this.worldArea.getBackupGrid();
        GenerateRandom rnd = new MersenneTwisterGenerateRandom();

        for (int row = 0; row < this.worldArea.getRows(); row++) {
            for (int col = 0; col < this.worldArea.getCols(); col++) {
                grid[row][col] = rnd.nextBoolean();
            }
        }

        this.worldArea.advanceBackupGrid();

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed(ActionEvent ev) {
        if (ev.getSource() == iterationButton) {
            performIteration();
            repaint();
        } else if (ev.getSource() == startButton) {
            performStart();
        } else if (ev.getSource() == stopButton) {
            performStop();
        } else if (ev.getSource() == resetButton) {
            performReset();
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowActivated(WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosed(WindowEvent arg0) {


    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosing(WindowEvent arg0) {

        System.exit(0);

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeactivated(WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeiconified(WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowIconified(WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowOpened(WindowEvent arg0) {
        performReset();
    }


    /**
     * Main entry point.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        try {
            JFrame f = new ConwayCA();
            f.setVisible(true);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {
        this.requestStop = false;
        while (!this.requestStop) {
            performIteration();
            this.repaint();
            try {
                Thread.sleep(100);
            } catch (InterruptedException ex) {
                ex.printStackTrace();
            }
        }

        // Update GUI
        this.iterationButton.setEnabled(true);
        this.stopButton.setEnabled(false);
        this.startButton.setEnabled(true);
    }
}
