/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

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

public class MultiverseViewer extends JFrame implements ActionListener,
        WindowListener, Runnable {

    private static ConfigData config = new ConfigData();
    public static final String CONFIG_NAME = ".multiverse_viewer.ser";

    /**
     * @return the config
     */
    public static ConfigData getConfig() {
        return MultiverseViewer.config;
    }

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

    public static void main(final String[] args) {
        try {
            final JFrame f = new MultiverseViewer();
            f.setVisible(true);
        } catch (final Exception ex) {
            ex.printStackTrace();
        }

    }

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

    private final JCheckBox autoKill;
    private final JButton configButton;
    private final JButton deselectButton;
    private final JButton goButton;

    private final DisplayPanel outputPanel;

    private final JButton resetButton;

    private boolean running = false;

    private final JButton startButton;

    private final JButton stopButton;

    private boolean stopRequest = false;

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
        this.configButton.addActionListener(this);
        this.deselectButton.addActionListener(this);

        this.outputPanel.setAutoKill(true);
        this.autoKill.setSelected(true);
        this.deselectButton.setEnabled(false);

        addWindowListener(this);
    }

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

    public void enableDeselect() {
        this.deselectButton.setEnabled(true);
    }

    public void performAutoKill() {
        this.outputPanel.setAutoKill(this.autoKill.isSelected());
    }

    private void performConfig() {
        final ConfigDialog dialog = new ConfigDialog();
        dialog.setModal(true);
        dialog.setVisible(true);
    }

    public void performDeselect() {
        this.outputPanel.deselect();
        this.deselectButton.setEnabled(false);
    }

    public void performReset() {
        this.outputPanel.resetAll();
    }

    public void performStart() {
        if (!this.running) {
            final Thread t = new Thread(this);
            t.start();
        }
    }

    public void performStop() {
        if (this.running) {
            this.stopRequest = true;
        }
    }

    @Override
    public void run() {
        this.running = true;

        while (!this.stopRequest) {
            this.outputPanel.update();
        }
        this.stopRequest = false;
        this.running = false;
    }

    @Override
    public void windowActivated(final WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowClosed(final WindowEvent arg0) {

    }

    @Override
    public void windowClosing(final WindowEvent arg0) {
        MultiverseViewer.saveConfig();
        System.exit(0);
    }

    @Override
    public void windowDeactivated(final WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowDeiconified(final WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowIconified(final WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowOpened(final WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

}