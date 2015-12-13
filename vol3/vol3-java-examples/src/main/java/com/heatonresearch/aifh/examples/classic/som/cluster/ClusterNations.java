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
package com.heatonresearch.aifh.examples.classic.som.cluster;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.fns.RBFEnum;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;
import com.heatonresearch.aifh.som.SelfOrganizingMap;
import com.heatonresearch.aifh.som.neighborhood.NeighborhoodRBF;
import com.heatonresearch.aifh.som.train.BasicTrainSOM;

import javax.swing.*;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Example of clustering nations, by several stats, using a SOM.
 * Use a hexagon map.
 */
public class ClusterNations extends JFrame implements Runnable {

    public static final int WIDTH = 8;
    public static final int HEIGHT = 8;
    public static final int START_WIDTH = 3;
    public static final int END_WIDTH = 1;
    public static final int CYCLES = 1000;
    public static final double START_RATE = 0.05;
    public static final double END_RATE = 0.001;


    private final HexPanel map;
    private final SelfOrganizingMap network;
    private final Thread thread;
    private final BasicTrainSOM train;
    private NeighborhoodRBF gaussian;
    private final int buckets;
    private List<BasicData> trainingData;

    public ClusterNations() {
        this.setSize(750, 300);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        loadTraining();

        this.gaussian = new NeighborhoodRBF(RBFEnum.Gaussian, ClusterNations.WIDTH,
                ClusterNations.HEIGHT);

        this.buckets = WIDTH * HEIGHT;
        this.network = new SelfOrganizingMap(3,this.buckets);
        this.network.reset();

        this.gaussian = new NeighborhoodRBF(RBFEnum.Gaussian,WIDTH,HEIGHT);
        this.gaussian.setHexagon(true);
        this.train = new BasicTrainSOM(this.network, 0.01, this.trainingData, this.gaussian);
        this.train.setAutoDecay(CYCLES,START_RATE,END_RATE,START_WIDTH,END_WIDTH);
        this.train.setForceWinner(false);

        this.getContentPane().add(this.map = new HexPanel(this.network,32,WIDTH,HEIGHT));
        this.map.setDisplayNumbers(true);

        this.thread = new Thread(this);
        this.thread.start();
    }

    /**
     * Run the example.
     */
    public void loadTraining() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/nations.csv");
            if (istream == null) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }

            GenerateRandom rnd = new MersenneTwisterGenerateRandom();

            final DataSet ds = DataSet.load(istream);
            ds.deleteUnknowns();

            ds.deleteColumn(0);
            ds.normalizeRange(1, 0, 1);
            ds.normalizeRange(2, 0, 1);
            ds.normalizeRange(3, 0, 1);
            istream.close();

            this.trainingData = ds.extractUnsupervisedLabeled(0);
        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    public static void main(String[] args) {
        ClusterNations nations = new ClusterNations();
        nations.setVisible(true);
    }

    @Override
    public void run() {

        for (int i = 0; i < CYCLES; i++) {
            this.train.iteration();
            this.train.autoDecay();
            this.map.repaint();
            System.out.println("Iteration " + i + "," + this.train.toString());
        }


        for(int i = 0; i< this.buckets; i++) {
            List<String> nations = new ArrayList<>();
            for(BasicData nation: this.trainingData) {
                if(this.network.classify(nation.getInput())==i ) {
                    nations.add(nation.getLabel());
                }
            }
            System.out.println("Cluster #" + i + ": " + nations.toString());
        }
    }
}
