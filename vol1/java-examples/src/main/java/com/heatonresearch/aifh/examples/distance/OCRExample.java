/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.examples.distance;

import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;

/**
 * OCR: Main form that allows the user to interact with the OCR application.
 */
public class OCRExample extends JFrame {

    /**
     * The downsample width for the application.
     */
    static final int DOWNSAMPLE_WIDTH = 5;
    /**
     * The down sample height for the application.
     */
    static final int DOWNSAMPLE_HEIGHT = 7;
    /**
     * Serial id for this class.
     */
    private static final long serialVersionUID = -6779380961875907013L;
    /**
     * The entry component for the user to draw into.
     */
    private final Entry entry;
    /**
     * The down sample component to display the drawing downsampled.
     */
    private final Sample sample;
    /**
     * The letters that have been defined.
     */
    private final DefaultListModel letterListModel = new DefaultListModel();
    /**
     * THe downsample button.
     */
    private final JButton downSample = new JButton();
    /**
     * The add button.
     */
    private final JButton add = new JButton();
    /**
     * The clear button
     */
    private final JButton clear = new JButton();
    /**
     * The recognize button
     */
    private final JButton recognize = new JButton();
    /**
     * The letters list box
     */
    private final JList letters = new JList();
    /**
     * The delete button
     */
    private final JButton del = new JButton();
    /**
     * The load button
     */
    private final JButton load = new JButton();
    /**
     * The save button
     */
    private final JButton save = new JButton();
    final JLabel JLabel3 = new JLabel();
    final JLabel JLabel8 = new JLabel();
    final JLabel JLabel5 = new JLabel();

    private final CalculateDistance distanceCalc = new EuclideanDistance();

    /**
     * The constructor.
     */
    OCRExample() {
        getContentPane().setLayout(null);
        this.entry = new Entry();
        this.entry.setLocation(168, 25);
        this.entry.setSize(200, 128);
        getContentPane().add(this.entry);

        this.sample = new Sample(OCRExample.DOWNSAMPLE_WIDTH, OCRExample.DOWNSAMPLE_HEIGHT);
        this.sample.setLocation(307, 210);
        this.sample.setSize(65, 70);

        this.entry.setSample(this.sample);
        getContentPane().add(this.sample);

        setTitle("OCR");
        getContentPane().setLayout(null);
        setSize(405, 382);
        setVisible(false);
        final JLabel JLabel1 = new JLabel();
        JLabel1.setText("Letters Known");
        getContentPane().add(JLabel1);
        JLabel1.setBounds(12, 12, 100, 12);
        final JLabel JLabel2 = new JLabel();
        JLabel2.setBounds(12, 264, 72, 24);
        this.downSample.setText("D Sample");
        this.downSample.setActionCommand("Down Sample");
        getContentPane().add(this.downSample);
        this.downSample.setBounds(252, 180, 120, 24);
        this.add.setText("Add");
        this.add.setActionCommand("Add");
        getContentPane().add(this.add);
        this.add.setBounds(168, 156, 84, 24);
        this.clear.setText("Clear");
        this.clear.setActionCommand("Clear");
        getContentPane().add(this.clear);
        this.clear.setBounds(168, 180, 84, 24);
        this.recognize.setText("Recognize");
        this.recognize.setActionCommand("Recognize");
        getContentPane().add(this.recognize);
        this.recognize.setBounds(252, 156, 120, 24);
        final JScrollPane JScrollPane1 = new JScrollPane();
        JScrollPane1
                .setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        JScrollPane1.setOpaque(true);
        getContentPane().add(JScrollPane1);
        JScrollPane1.setBounds(12, 24, 144, 132);
        JScrollPane1.getViewport().add(this.letters);
        this.letters.setBounds(0, 0, 126, 129);
        this.del.setText("Delete");
        this.del.setActionCommand("Delete");
        getContentPane().add(this.del);
        this.del.setBounds(12, 156, 144, 24);
        this.load.setText("Load");
        this.load.setActionCommand("Load");
        getContentPane().add(this.load);
        this.load.setBounds(12, 180, 75, 24);
        this.save.setText("Save");
        this.save.setActionCommand("Save");
        getContentPane().add(this.save);
        this.save.setBounds(84, 180, 72, 24);
        this.JLabel3.setBounds(12, 288, 72, 24);
        this.JLabel8.setHorizontalTextPosition(SwingConstants.CENTER);
        this.JLabel8.setHorizontalAlignment(SwingConstants.CENTER);
        this.JLabel8.setFont(new Font("Dialog", Font.BOLD, 14));
        this.JLabel8.setBounds(12, 240, 120, 24);
        this.JLabel5.setText("Draw Letters Here");
        getContentPane().add(this.JLabel5);
        this.JLabel5.setBounds(204, 12, 144, 12);

        final SymAction lSymAction = new SymAction();
        this.downSample.addActionListener(lSymAction);
        this.clear.addActionListener(lSymAction);
        this.add.addActionListener(lSymAction);
        this.del.addActionListener(lSymAction);
        final SymListSelection lSymListSelection = new SymListSelection();
        this.letters.addListSelectionListener(lSymListSelection);
        this.load.addActionListener(lSymAction);
        this.save.addActionListener(lSymAction);
        this.recognize.addActionListener(lSymAction);
        this.letters.setModel(this.letterListModel);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    /**
     * The main method.
     *
     * @param args Args not really used.
     */
    public static void main(final String args[]) {
        (new OCRExample()).setVisible(true);
    }

    /**
     * Called to add the current image to the training set
     */
    void add_actionPerformed() {
        int i;

        final String letter = JOptionPane
                .showInputDialog("Please enter a letter you would like to assign this sample to.");
        if (letter == null) {
            return;
        }

        if (letter.length() > 1) {
            JOptionPane.showMessageDialog(this,
                    "Please enter only a single letter.", "Error",
                    JOptionPane.ERROR_MESSAGE);
            return;
        }

        this.entry.downSample();
        final SampleData sampleData = (SampleData) this.sample.getData()
                .clone();
        sampleData.setLetter(letter.charAt(0));

        for (i = 0; i < this.letterListModel.size(); i++) {
            final String str = "" + ((SampleData) this.letterListModel
                    .getElementAt(i)).getLetter();
            if (str.equals(letter)) {
                JOptionPane.showMessageDialog(this,
                        "That letter is already defined, delete it first!",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return;
            }

            final String l = "" + sampleData.getLetter();
            if (str.compareTo(l) > 0) {
                this.letterListModel.add(i, sampleData);
                return;
            }
        }
        this.letterListModel.add(this.letterListModel.size(), sampleData);
        this.entry.clear();
        this.sample.repaint();

    }

    /**
     * Called to clear the image.
     */
    void clear_actionPerformed() {
        this.entry.clear();
        this.sample.getData().clear();
        this.sample.repaint();

    }

    /**
     * Called when the del button is pressed.
     */
    void del_actionPerformed() {
        final int i = this.letters.getSelectedIndex();

        if (i == -1) {
            JOptionPane.showMessageDialog(this,
                    "Please select a letter to delete.", "Error",
                    JOptionPane.ERROR_MESSAGE);
            return;
        }

        this.letterListModel.remove(i);
    }

    /**
     * Called to downsample the image.
     */
    void downSample_actionPerformed() {
        this.entry.downSample();

    }

    /**
     * Called when a letter is selected from the list box.
     */
    void letters_valueChanged() {
        if (this.letters.getSelectedIndex() == -1) {
            return;
        }
        final SampleData selected = (SampleData) this.letterListModel
                .getElementAt(this.letters.getSelectedIndex());
        this.sample.setData((SampleData) selected.clone());
        this.sample.repaint();
        this.entry.clear();

    }

    /**
     * Called when the load button is pressed.
     */
    void load_actionPerformed() {
        try {
            final FileReader f;// the actual file stream
            final BufferedReader r;// used to read the file line by line

            f = new FileReader(new File("./sample.dat"));
            r = new BufferedReader(f);
            String line;
            int i = 0;

            this.letterListModel.clear();

            while ((line = r.readLine()) != null) {
                final SampleData ds = new SampleData(line.charAt(0),
                        OCRExample.DOWNSAMPLE_WIDTH, OCRExample.DOWNSAMPLE_HEIGHT);
                this.letterListModel.add(i++, ds);
                int idx = 2;
                for (int y = 0; y < ds.getHeight(); y++) {
                    for (int x = 0; x < ds.getWidth(); x++) {
                        ds.setData(x, y, line.charAt(idx++) == '1');
                    }
                }
            }

            r.close();
            f.close();
            clear_actionPerformed();
            JOptionPane.showMessageDialog(this, "Loaded from 'sample.dat'.",
                    "Training", JOptionPane.PLAIN_MESSAGE);

        } catch (final Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog(this, "Error: " + e, "Training",
                    JOptionPane.ERROR_MESSAGE);
        }

    }

    /**
     * Called when the recognize button is pressed.
     */
    void recognize_actionPerformed() {

        this.entry.downSample();

        double bestPosition = Double.POSITIVE_INFINITY;
        String letter = "?";

        final double[] letterToRecognize = this.sample.getData().getPosition();

        for (int i = 0; i < this.letterListModel.size(); i++) {
            final SampleData ds = (SampleData) this.letterListModel
                    .getElementAt(i);

            final double dist = this.distanceCalc.calculate(letterToRecognize, ds.getPosition());
            if (dist < bestPosition) {
                bestPosition = dist;
                letter = "" + ds.getLetter();
            }
        }


        JOptionPane
                .showMessageDialog(this, letter, "That Letter Is",
                        JOptionPane.PLAIN_MESSAGE);
        clear_actionPerformed();

    }

    /**
     * Called when the save button is clicked.
     */
    void save_actionPerformed() {
        try {
            final OutputStream os;// the actual file stream
            final PrintStream ps;// used to read the file line by line

            os = new FileOutputStream("./sample.dat", false);
            ps = new PrintStream(os);

            for (int i = 0; i < this.letterListModel.size(); i++) {
                final SampleData ds = (SampleData) this.letterListModel
                        .elementAt(i);
                ps.print(ds.getLetter() + ":");
                for (int y = 0; y < ds.getHeight(); y++) {
                    for (int x = 0; x < ds.getWidth(); x++) {
                        ps.print(ds.getData(x, y) ? "1" : "0");
                    }
                }
                ps.println("");
            }

            ps.close();
            os.close();
            clear_actionPerformed();
            JOptionPane.showMessageDialog(this, "Saved to 'sample.dat'.",
                    "Training", JOptionPane.PLAIN_MESSAGE);

        } catch (final Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog(this, "Error: " + e, "Training",
                    JOptionPane.ERROR_MESSAGE);
        }

    }

    class SymAction implements java.awt.event.ActionListener {
        public void actionPerformed(final ActionEvent event) {
            final Object object = event.getSource();
            if (object == OCRExample.this.downSample) {
                downSample_actionPerformed();
            } else if (object == OCRExample.this.clear) {
                clear_actionPerformed();
            } else if (object == OCRExample.this.add) {
                add_actionPerformed();
            } else if (object == OCRExample.this.del) {
                del_actionPerformed();
            } else if (object == OCRExample.this.save) {
                save_actionPerformed();
            } else if (object == OCRExample.this.load) {
                load_actionPerformed();
            } else if (object == OCRExample.this.recognize) {
                recognize_actionPerformed();
            }
        }
    }

    class SymListSelection implements javax.swing.event.ListSelectionListener {
        public void valueChanged(
                final javax.swing.event.ListSelectionEvent event) {
            final Object object = event.getSource();
            if (object == OCRExample.this.letters) {
                letters_valueChanged();
            }
        }
    }

}
