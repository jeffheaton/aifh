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
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.*;

/**
 * This example shows how to use a Human Based Genetic Algorithm to evolve a set of physical constants to create
 * an interesting cellular automation. You are shown a multiverse, with many different running universes. You can
 * choose universes that look interesting, and experiment with crossover and mutation to create more interesting
 * universes.
 * <p/>
 * For complete instructions, refer to the following article.
 * <p/>
 * http://www.codeproject.com/Articles/730362/Using-an-evolutionary-algorithm-to-create-a-cellul
 * <p/>
 * Other references.
 * <p/>
 * http://en.wikipedia.org/wiki/Human-based_genetic_algorithm
 * http://en.wikipedia.org/wiki/Cellular_automaton
 */
public class MultiverseViewer extends JFrame implements ActionListener,
        WindowListener, Runnable {

    /**
     * Configuration data.
     */
    private static ConfigData config = new ConfigData();

    /**
     * The file that holds configuration data.
     */
    public static final String CONFIG_NAME = ".multiverse_viewer.ser";

    /**
     * @return the config
     */
    public static ConfigData getConfig() {
        return MultiverseViewer.config;
    }

    /**
     * Load the configuration data.
     */
    private static void loadConfig() {
        try {
            // find the config file in the home directory
            final File home = new File(System.getProperty("user.home"));
            final File configFile = new File(home, MultiverseViewer.CONFIG_NAME);

            // load the config file
            final FileInputStream fileIn = new FileInputStream(configFile);
            final ObjectInputStream in = new ObjectInputStream(fileIn);
            MultiverseViewer.config = (ConfigData) in.readObject();
            in.close();
            fileIn.close();
        } catch (final IOException ex) {
            MultiverseViewer.config = new ConfigData();
        } catch (final ClassNotFoundException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Main entry point.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        try {
            final JFrame f = new MultiverseViewer();
            f.setVisible(true);
        } catch (final Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * Save the configuration file.
     */
    public static void saveConfig() {
        try {
            // find the config file in the home directory
            final File home = new File(System.getProperty("user.home"));
            final File configFile = new File(home, MultiverseViewer.CONFIG_NAME);

            // load the config file
            final FileOutputStream fileOut = new FileOutputStream(configFile);
            final ObjectOutputStream out = new ObjectOutputStream(fileOut);
            out.writeObject(MultiverseViewer.config);
            out.close();
            fileOut.close();
        } catch (final IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * @param config the config to set
     */
    public static void setConfig(final ConfigData config) {
        MultiverseViewer.config = config;
    }

    /**
     * Allow universes to be auto-killed if they stop moving.
     */
    private final JCheckBox autoKill;

    /**
     * Open the configuration dialog.
     */
    private final JButton configButton;

    /**
     * Deselect any universes.
     */
    private final JButton deselectButton;

    /**
     * Simulate a single frame.
     */
    private final JButton goButton;

    /**
     * The multiverse view panel.
     */
    private final DisplayPanel outputPanel;

    /**
     * Reset all universes.
     */
    private final JButton resetButton;

    /**
     * Are the universes running?
     */
    private boolean running = false;

    /**
     * Start all universes.
     */
    private final JButton startButton;

    /**
     * Stop all universes.
     */
    private final JButton stopButton;

    /**
     * Has a stop been requested?
     */
    private boolean stopRequest = false;

    /**
     * The constructor.
     */
    public MultiverseViewer() {
        MultiverseViewer.loadConfig();
        setSize(1024, 768);
        setTitle("Merge Life - Multiverse!");

        final int rows = MultiverseViewer.getConfig().getUniversePaneRows();
        final int cols = MultiverseViewer.getConfig().getUniversePaneColumns();

        final Container c = getContentPane();
        c.setLayout(new BorderLayout());
        final JPanel buttonPanel = new JPanel();
        c.add(buttonPanel, BorderLayout.NORTH);

        buttonPanel.add(this.configButton = new JButton("Config..."));
        buttonPanel.add(this.goButton = new JButton("Single Step"));
        buttonPanel.add(this.startButton = new JButton("Start"));
        buttonPanel.add(this.stopButton = new JButton("Stop"));
        buttonPanel.add(this.resetButton = new JButton("Reset"));
        buttonPanel.add(this.autoKill = new JCheckBox("Auto Kill"));
        buttonPanel.add(this.deselectButton = new JButton("Deselect"));

        c.add(this.outputPanel = new DisplayPanel(this, rows, cols),
                BorderLayout.CENTER);
        this.goButton.addActionListener(this);
        this.startButton.addActionListener(this);
        this.stopButton.addActionListener(this);
        this.resetButton.addActionListener(this);
        this.configButton.addActionListener(this);
        this.autoKill.addActionListener(this);
        this.deselectButton.addActionListener(this);

        this.outputPanel.setAutoKill(true);
        this.autoKill.setSelected(true);
        this.deselectButton.setEnabled(false);

        addWindowListener(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed(final ActionEvent ev) {
        if (ev.getSource() == this.goButton) {
            this.outputPanel.update();
        } else if (ev.getSource() == this.startButton) {
            performStart();
        } else if (ev.getSource() == this.stopButton) {
            performStop();
        } else if (ev.getSource() == this.resetButton) {
            performReset();
        } else if (ev.getSource() == this.configButton) {
            performConfig();
        } else if (ev.getSource() == this.autoKill) {
            performAutoKill();
        } else if (ev.getSource() == this.deselectButton) {
            performDeselect();
        }
    }

    /**
     * Allow the deselect button.
     */
    public void enableDeselect() {
        this.deselectButton.setEnabled(true);
    }

    /**
     * Set auto kill mode.
     */
    public void performAutoKill() {
        this.outputPanel.setAutoKill(this.autoKill.isSelected());
    }

    /**
     * Open the config dialog.
     */
    private void performConfig() {
        final ConfigDialog dialog = new ConfigDialog();
        dialog.setModal(true);
        dialog.setVisible(true);
    }

    /**
     * Deselect any selected universe.
     */
    public void performDeselect() {
        this.outputPanel.deselect();
        this.deselectButton.setEnabled(false);
    }

    /**
     * Perform a reset of the universe.
     */
    public void performReset() {
        this.outputPanel.resetAll();
    }

    /**
     * Start the universes.
     */
    public void performStart() {
        if (!this.running) {
            final Thread t = new Thread(this);
            t.start();
        }
    }

    /**
     * Stop the universes.
     */
    public void performStop() {
        if (this.running) {
            this.stopRequest = true;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {
        this.running = true;

        while (!this.stopRequest) {
            this.outputPanel.update();
        }
        this.stopRequest = false;
        this.running = false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowActivated(final WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosed(final WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosing(final WindowEvent arg0) {
        MultiverseViewer.saveConfig();
        System.exit(0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeactivated(final WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeiconified(final WindowEvent arg0) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowIconified(final WindowEvent arg0) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowOpened(final WindowEvent arg0) {

    }

}
