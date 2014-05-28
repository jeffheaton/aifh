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

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

/**
 * This example implements an elementary cellular automation.
 * <p/>
 * References:
 * http://mathworld.wolfram.com/ElementaryCellularAutomaton.html
 */
public class ElementaryCA extends JFrame implements ActionListener, WindowListener {
    /**
     * The generate button.
     */
    private JButton generateButton;

    /**
     * The world area.
     */
    private WorldPanel worldArea;

    /**
     * Allow rule # to be entered.
     */
    private TextField ruleInput;

    /**
     * The number of rows.
     */
    public static final int ROWS = 200;

    /**
     * The number of columns.
     */
    public static final int COLS = 200;

    /**
     * The constructor.
     */
    public ElementaryCA() {
        setSize(500, 500);
        setTitle("Elementary Cellular Automation");

        Container c = getContentPane();
        c.setLayout(new BorderLayout());
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        c.add(buttonPanel, BorderLayout.NORTH);

        buttonPanel.add(new Label("Rule (0-255):"));
        buttonPanel.add(this.ruleInput = new TextField(5));
        this.ruleInput.setText("30");
        buttonPanel.add(generateButton = new JButton("Generate"));

        this.worldArea = new WorldPanel(ROWS, COLS, false);
        /*
      Allow scrolling.
     */
        final JScrollPane scroll = new JScrollPane(this.worldArea);
        c.add(scroll, BorderLayout.CENTER);
        generateButton.addActionListener(this);

        this.addWindowListener(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed(ActionEvent ev) {
        if (ev.getSource() == generateButton) {
            performGenerate();
            repaint();
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowActivated(WindowEvent arg0) {
        // TODO Auto-generated method stub

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
        // TODO Auto-generated method stub

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeiconified(WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowIconified(WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowOpened(WindowEvent arg0) {
        performGenerate();
    }


    /**
     * The main entry point.
     *
     * @param args The arguments.
     */
    public static void main(String[] args) {
        try {
            JFrame f = new ElementaryCA();
            f.setVisible(true);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * Generate the CA.
     */
    public void performGenerate() {
        boolean invalid = false;
        boolean[] output = new boolean[8];
        int center = this.worldArea.getCols() / 2;
        this.worldArea.getPrimaryGrid()[0][center] = true;
        boolean[][] grid = this.worldArea.getPrimaryGrid();

        // Default to rule 30
        int rule = 30;

        try {
            rule = Integer.parseInt(this.ruleInput.getText());
            if (rule < 0 || rule > 255) {
                invalid = true;
            }
        } catch (NumberFormatException ex) {
            invalid = true;
        }

        if (invalid) {
            JOptionPane.showMessageDialog(null, "Invalid rule number, must be between 0 and 255.");
            return;
        }

        int cx = 1;
        int idx = 7;
        while (idx > 0) {
            output[idx--] = (rule & cx) != 0;
            cx *= 2;
        }

        for (int row = 1; row < this.worldArea.getRows(); row++) {
            int prevRow = row - 1;

            for (int i = 0; i < this.worldArea.getCols() - 2; i++) {
                boolean result = false;
                boolean a = grid[prevRow][i];
                boolean b = grid[prevRow][i + 1];
                boolean c = grid[prevRow][i + 2];

                if (a && b && c) {
                    result = output[0];
                } else if (a && b && !c) {
                    result = output[1];
                } else if (a && !b && c) {
                    result = output[2];
                } else if (a && !b && !c) {
                    result = output[3];
                } else if (!a && b && c) {
                    result = output[4];
                } else if (!a && b && !c) {
                    result = output[5];
                } else if (!a && !b && c) {
                    result = output[6];
                } else if (!a && !b && !c) {
                    result = output[7];
                }

                grid[row][i + 1] = result;
            }
        }


        this.repaint();
    }
}
