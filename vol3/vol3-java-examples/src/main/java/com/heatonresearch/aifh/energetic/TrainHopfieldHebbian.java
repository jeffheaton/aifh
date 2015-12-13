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

import com.heatonresearch.aifh.AIFHError;

/**
 * Train the Hopfield network using a Hebbian algorithm.
 * For more info: https://en.wikipedia.org/wiki/Hopfield_network
 */
public class TrainHopfieldHebbian {

    /**
     * The network to train.
     */
    private final HopfieldNetwork network;

    /**
     * The summation matrix.
     */
    private final double[][] sumMatrix;

    /**
     * The count of patterns.
     */
    private int patternCount;

    /**
     * Construct the trainer.
     * @param theNetwork The network to train.
     */
    public TrainHopfieldHebbian(HopfieldNetwork theNetwork) {
        this.network = theNetwork;
        this.sumMatrix = new double[this.network.getInputCount()][this.network.getInputCount()];
    }

    /**
     * Add a pattern to train.
     * @param pattern The pattern to train.
     */
    public void addPattern(double[] pattern) {
        for(int i=0;i<this.sumMatrix.length;i++) {
            for(int j=0;j<this.sumMatrix.length;j++) {
                if(i==j) {
                    this.sumMatrix[i][j] = 0;
                } else {
                    this.sumMatrix[i][j] += pattern[i] * pattern[j];
                }
            }
        }
        this.patternCount++;
    }

    /**
     * Learn the added patterns.
     */
    public void learn() {
        if( this.patternCount==0) {
            throw new AIFHError("Please add a pattern before learning.  Nothing to learn.");
        }

        for(int i=0;i<this.sumMatrix.length;i++) {
            for(int j=0;j<this.sumMatrix.length;j++) {
                this.network.setWeight(i,j,this.sumMatrix[i][j]/this.patternCount);
            }
        }
    }

}
