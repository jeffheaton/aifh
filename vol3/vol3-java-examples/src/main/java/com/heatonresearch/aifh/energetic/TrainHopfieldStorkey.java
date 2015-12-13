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
package com.heatonresearch.aifh.energetic;

/**
 * Train the Hopfield network using a Storkey algorithm.
 * For more info: https://en.wikipedia.org/wiki/Hopfield_network
 */
public class TrainHopfieldStorkey {
    /**
     * The network to train.
     */
    private final HopfieldNetwork network;

    /**
     * The summation matrix.
     */
    private final double[][] sumMatrix;

    public TrainHopfieldStorkey(HopfieldNetwork theNetwork) {
        this.network = theNetwork;
        this.sumMatrix = new double[this.network.getInputCount()][this.network.getInputCount()];
    }

    /**
     * Calculate the local field needed by training.
     * @param i The neuron.
     * @param pattern The pattern.
     * @return The local field value.
     */
    private double calculateLocalField(int i, double[] pattern) {
        double sum = 0;
        for(int k=0;k<this.network.getInputCount();k++) {
            if(k!=i) {
                sum += this.network.getWeight(i,k) * pattern[k];
            }
        }
        return sum;
    }

    /**
     * Add a pattern for training.
     * @param pattern The pattern to add.
     */
    public void addPattern(double[] pattern) {
        for(int i = 0; i< this.sumMatrix.length; i++) {
            for(int j = 0; j< this.sumMatrix.length; j++) {
                this.sumMatrix[i][j] = 0;
            }
        }

        double n = this.network.getInputCount();
        for(int i=0;i<this.sumMatrix.length;i++) {
            for(int j=0;j<this.sumMatrix.length;j++) {
                double t1 = (pattern[i] * pattern[j])/n;
                double t2 = (pattern[i] * calculateLocalField(j,pattern))/n;
                double t3 = (pattern[j] * calculateLocalField(i,pattern))/n;
                double d = t1-t2-t3;
                this.sumMatrix[i][j]+=d;
            }
        }

        for(int i = 0; i< this.sumMatrix.length; i++) {
            for(int j = 0; j< this.sumMatrix.length; j++) {
                this.network.setWeight(i,j, this.network.getWeight(i,j)+this.sumMatrix[i][j]);
            }
        }
    }


}
