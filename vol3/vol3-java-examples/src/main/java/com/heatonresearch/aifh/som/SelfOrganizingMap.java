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
package com.heatonresearch.aifh.som;

import Jama.Matrix;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

public class SelfOrganizingMap {

    /**
     *
     */
    private Matrix weights;

    private final CalculateDistance calcDist = new EuclideanDistance();

    /**
     * The constructor.
     *
     * @param inputCount
     *            Number of input neurons
     * @param outputCount
     *            Number of output neurons
     */
    public SelfOrganizingMap(final int inputCount, final int outputCount) {
        this.weights = new Matrix(outputCount,inputCount);
    }


    public double calculateError(final double[][] data) {

        final BestMatchingUnit bmu = new BestMatchingUnit(this);

        bmu.reset();

        // Determine the BMU for each training element.
        for (final double[] pair : data) {
            final double[] input = pair;
            bmu.calculateBMU(input);
        }

        // update the error
        return bmu.getWorstDistance() / 100.0;
    }

    /**
     * {@inheritDoc}
     */
    public int classify(final double[] input) {
        if (input.length > getInputCount()) {
            throw new AIFHError(
                    "Can't classify SOM with input size of " + getInputCount()
                            + " with input data of count " + input.length);
        }

        double minDist = Double.POSITIVE_INFINITY;
        int result = -1;

        for (int i = 0; i < getOutputCount(); i++) {
            double dist = this.calcDist.calculate(input, this.weights.getArray()[i]);
            if (dist < minDist) {
                minDist = dist;
                result = i;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     */
    public int getInputCount() {
        return this.weights.getColumnDimension();
    }

    /**
     * {@inheritDoc}
     */
    public int getOutputCount() {
        return this.weights.getRowDimension();
    }

    /**
     * @return the weights
     */
    public Matrix getWeights() {
        return this.weights;
    }


    public void reset(GenerateRandom rnd) {
        for(int i=0;i<this.weights.getRowDimension();i++) {
            for(int j=0;j<this.weights.getColumnDimension();j++) {
                this.weights.set(i,j,rnd.nextDouble(-1,1));
            }
        }
    }

    public void reset() {
        reset(new MersenneTwisterGenerateRandom());
    }



    /**
     * @param weights
     *            the weights to set
     */
    public void setWeights(final Matrix weights) {
        this.weights = weights;
    }

    /**
     * An alias for the classify method, kept for compatibility
     * with earlier versions of Encog.
     *
     * @param input
     *            The input pattern.
     * @return The winning neuron.
     */
    public int winner(final double[] input) {
        return classify(input);
    }
}
