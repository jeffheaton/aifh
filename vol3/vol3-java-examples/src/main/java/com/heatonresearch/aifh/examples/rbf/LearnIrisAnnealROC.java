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
package com.heatonresearch.aifh.examples.rbf;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainAnneal;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class LearnIrisAnnealROC extends JFrame implements ActionListener {

    private final JButton buttonAnneal;
    private final JButton buttonReset;
    private final TrainAnneal trainer;
    private final RBFNetwork network;
    private final List<BasicData> training;
    private final XYSeriesCollection dataset;
    private final XYSeries dataSeries1;
    public static final String TPR = "True Positive Rate";
    public static final String FPR = "False Positive Rate";
    public static final String THRESHOLD = "threshold";

    class RocPair implements Comparable<RocPair> {
        private final double x;
        private final double y;

        public RocPair(double x, double y) {
            this.x = x;
            this.y = y;
        }

        public double getX() {
            return this.x;
        }

        public double getY() {
            return this.y;
        }

        public boolean equal(RocPair other) {
            return Double.compare(getX(),other.getX())==0;
        }

        @Override
        public int compareTo(RocPair other) {
            return Double.compare(getX(),other.getX());
        }

        @Override
        public String toString() {
            return "[" + this.x + "," + this.y + "]";
        }
    }

    public LearnIrisAnnealROC() {
        this.setSize(640, 480);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setTitle("Iris Classify ROC");
        Container content = this.getContentPane();
        content.setLayout(new BorderLayout());

        this.dataset = new XYSeriesCollection( );
        this.dataSeries1 = new XYSeries("Threshold");
        this.dataset.addSeries(this.dataSeries1);

        final JFreeChart lineChart = ChartFactory.createXYLineChart(
                "Line Chart Demo 6",      // chart title
                FPR,                      // x axis label
                TPR,                      // y axis label
                this.dataset,                  // data
                PlotOrientation.VERTICAL,
                true,                     // include legend
                true,                     // tooltips
                false                     // urls
        );

        ChartPanel chartPanel = new ChartPanel( lineChart );
        chartPanel.setPreferredSize( new java.awt.Dimension( 560 , 367 ) );
        content.add(chartPanel, BorderLayout.CENTER);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(this.buttonAnneal = new JButton("Anneal"));
        buttonPanel.add(this.buttonReset = new JButton("Reset"));
        this.buttonAnneal.addActionListener(this);
        this.buttonReset.addActionListener(this);
        content.add(buttonPanel,BorderLayout.SOUTH);

        this.training = loadIrisData();

        this.network = new RBFNetwork(4, 4, 1);
        this.network.reset(new MersenneTwisterGenerateRandom());

        final ScoreFunction score = new ScoreRegressionData(this.training);
        this.trainer = new TrainAnneal(this.network, score);
    }

    private List<BasicData> loadIrisData() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if (istream == null) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, 0, 1);
            ds.normalizeRange(1, 0, 1);
            ds.normalizeRange(2, 0, 1);
            ds.normalizeRange(3, 0, 1);
            final Map<String, Integer> species = ds.encodeNumeric(4);
            istream.close();

            int irisVersicolor = species.get("Iris-versicolor");

            final java.util.List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 1);

            for (BasicData aTrainingData : trainingData) {
                if (aTrainingData.getIdeal()[0] == irisVersicolor) {
                    aTrainingData.getIdeal()[0] = 1; // True, is versicolor
                } else {
                    aTrainingData.getIdeal()[0] = 0; // False, is not versicolor
                }

            }

            return trainingData;

        } catch(IOException ex) {
            ex.printStackTrace();
            System.exit(0);
            return null;
        }
    }

    private double[] calculateTruePositiveFalsePositive(double thresh) {
        int tp = 0;
        int fp = 0;
        int tn = 0;
        int fn = 0;

        for(BasicData item: this.training) {
            double x = this.network.computeRegression(item.getInput())[0];
            double y = item.getIdeal()[0];
            if( x>thresh ) {
                if( y>0.5 ) {
                    tp++;
                } else {
                    fp++;
                }
            } else {
                if( y<0.5 ) {
                    tn++;
                } else {
                    fn++;
                }
            }
        }
        double tpr = ((double)tp)/(tp+fn);
        double fpr = ((double)fp)/(fp+tn);
        double[] result = new double[2];
        result[0] = fpr;
        result[1] = tpr;
        return result;
    }

    private void updateChart( )
    {
        for(int i=0;i<5;i++) {
            this.trainer.iteration();
        }
        System.out.println(this.trainer.getLastError());

        Set<RocPair> list = new TreeSet<>();
        list.add(new RocPair(0,0));
        for(int i=0;i<=10;i++) {
            double[] tpfp = calculateTruePositiveFalsePositive(i/10.0);
            list.add(new RocPair(tpfp[0],tpfp[1]));

        }
        list.add(new RocPair(1,1));

        this.dataSeries1.clear();
        for(RocPair pair: list) {
            this.dataSeries1.add( pair.getX(), pair.getY() );
        }
    }

    public static void main(String[] args) {
        JFrame prg = new LearnIrisAnnealROC();
        prg.setVisible(true);
    }

    /**
     * Invoked when an action occurs.
     *
     * @param e
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if( e.getSource()==this.buttonAnneal ) {
            updateChart();
        } else if(e.getSource()==this.buttonReset) {
            this.network.reset(new MersenneTwisterGenerateRandom());
            updateChart();
        }
    }
}
