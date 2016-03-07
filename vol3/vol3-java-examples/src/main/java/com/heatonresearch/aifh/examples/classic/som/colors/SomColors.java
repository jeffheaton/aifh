/*
 * Encog(tm) Java Examples v3.3
 * http://www.heatonresearch.com/encog/
 * https://github.com/encog/encog-java-examples
 *
 * Copyright 2008-2014 Heaton Research, Inc.
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
package com.heatonresearch.aifh.examples.classic.som.colors;

import com.heatonresearch.aifh.general.fns.RBFEnum;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;
import com.heatonresearch.aifh.som.SelfOrganizingMap;
import com.heatonresearch.aifh.som.neighborhood.NeighborhoodRBF;
import com.heatonresearch.aifh.som.train.BasicTrainSOM;

import javax.swing.*;

/**
 * A classic SOM example that shows how the SOM groups similar color shades.
 *
 */
public class SomColors extends JFrame implements Runnable {

    /**
     *
     */
    private static final long serialVersionUID = -6762179069967224817L;
    public static final int WIDTH = 50;
    public static final int HEIGHT = 50;
    private final MapPanel map;
    private final SelfOrganizingMap network;
    private final Thread thread;
    private final BasicTrainSOM train;
    private final NeighborhoodRBF gaussian;

    public SomColors() {
        this.setSize(640, 480);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.network = createNetwork();
        this.getContentPane().add(this.map = new MapPanel(this.network,8,WIDTH,HEIGHT));
        this.gaussian = new NeighborhoodRBF(RBFEnum.Gaussian,WIDTH,HEIGHT);
        this.train = new BasicTrainSOM(this.network, 0.01, null, this.gaussian);
        this.train.setForceWinner(false);
        this.thread = new Thread(this);
        this.thread.start();
    }

    public SelfOrganizingMap getNetwork() {
        return this.network;
    }

    private SelfOrganizingMap createNetwork() {
        SelfOrganizingMap result = new SelfOrganizingMap(3,WIDTH * HEIGHT);
        result.reset();
        return result;
    }

    public static void main(String[] args) {
        SomColors frame = new SomColors();
        frame.setVisible(true);
    }

    public void run() {
        GenerateRandom rnd = new MersenneTwisterGenerateRandom();

        double[][] samples = new double[15][3];
        for (int i = 0; i < 15; i++) {
            samples[i][0] = rnd.nextDouble(-1,1);
            samples[i][1] = rnd.nextDouble(-1,1);
            samples[i][2] = rnd.nextDouble(-1,1);
        }

        this.train.setAutoDecay(1000, 0.8, 0.003, 30, 5);

        for (int i = 0; i < 1000; i++) {
            int idx = (int) (Math.random() * samples.length);
            double[] c = samples[idx];

            this.train.trainPattern(c);
            this.train.autoDecay();
            this.map.repaint();
            System.out.println("Iteration " + i + "," + this.train.toString());
        }
    }
}
