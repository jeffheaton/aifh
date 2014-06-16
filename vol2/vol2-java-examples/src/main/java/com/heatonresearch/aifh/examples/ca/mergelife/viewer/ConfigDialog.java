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

public class ConfigDialog extends JDialog implements ActionListener {

    private final JButton buttonCancel;
    private final JButton buttonDefaults;
    private final JButton buttonOK;
    private final JTextField textPaneHeight;
    private final JTextField textPaneWidth;
    private final JTextField textUniversePaneColumns;
    private final JTextField textUniversePaneRows;
    private final JTextField textZoom;

    public ConfigDialog() {
        setTitle("Configuration");
        setSize(640, 480);

        final JPanel pannelButtons = new JPanel();
        setLayout(new BorderLayout());
        pannelButtons.setLayout(new FlowLayout());
        pannelButtons.add(this.buttonOK = new JButton("OK"));
        pannelButtons.add(this.buttonDefaults = new JButton("Defaults"));
        pannelButtons.add(this.buttonCancel = new JButton("Cancel"));
        add(pannelButtons, BorderLayout.SOUTH);

        final JPanel panelContent = new JPanel();
        panelContent.setLayout(new GridLayout(5, 2));
        add(panelContent, BorderLayout.CENTER);
        panelContent.add(new JLabel("Universe Pane Height:"));
        panelContent.add(this.textPaneHeight = new JTextField());
        panelContent.add(new JLabel("Universe Pane Width:"));
        panelContent.add(this.textPaneWidth = new JTextField());
        panelContent.add(new JLabel("Universe Pane Rows"));
        panelContent.add(this.textUniversePaneRows = new JTextField());
        panelContent.add(new JLabel("Universe Pane Columns"));
        panelContent.add(this.textUniversePaneColumns = new JTextField());
        panelContent.add(new JLabel("Zoom:"));
        panelContent.add(this.textZoom = new JTextField());

        setFields();

        this.buttonOK.addActionListener(this);
        this.buttonCancel.addActionListener(this);
        this.buttonDefaults.addActionListener(this);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == this.buttonOK) {
            MultiverseViewer.getConfig().setPaneHeight(
                    Integer.parseInt(this.textPaneHeight.getText()));
            MultiverseViewer.getConfig().setPaneWidth(
                    Integer.parseInt(this.textPaneWidth.getText()));
            MultiverseViewer.getConfig().setUniversePaneRows(
                    Integer.parseInt(this.textUniversePaneRows.getText()));
            MultiverseViewer.getConfig().setUniversePaneColumns(
                    Integer.parseInt(this.textUniversePaneColumns.getText()));
            MultiverseViewer.getConfig().setZoom(
                    Integer.parseInt(this.textZoom.getText()));
            MultiverseViewer.saveConfig();
            dispose();
            JOptionPane.showMessageDialog(null,"Please restart the application to switch to your new configuration settings.");
        } else if (e.getSource() == this.buttonDefaults) {
            MultiverseViewer.setConfig(new ConfigData());
            setFields();
        } else if (e.getSource() == this.buttonCancel) {
            dispose();
        }
    }

    private void setFields() {
        this.textPaneHeight.setText(""
                + MultiverseViewer.getConfig().getPaneHeight());
        this.textPaneWidth.setText(""
                + MultiverseViewer.getConfig().getPaneWidth());
        this.textUniversePaneRows.setText(""
                + MultiverseViewer.getConfig().getUniversePaneRows());
        this.textUniversePaneColumns.setText(""
                + MultiverseViewer.getConfig().getUniversePaneColumns());
        this.textZoom.setText("" + MultiverseViewer.getConfig().getZoom());
    }

}
