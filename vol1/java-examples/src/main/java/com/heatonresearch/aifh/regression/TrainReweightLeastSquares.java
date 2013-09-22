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

package com.heatonresearch.aifh.regression;

import Jama.LUDecomposition;
import Jama.Matrix;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.data.BasicData;

import java.util.List;

/**
 * Train a GLM using iteratively reweighted least squares.
 * <p/>
 * http://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
 */
public class TrainReweightLeastSquares {

    /**
     * The GLM to train.
     */
    private final MultipleLinearRegression algorithm;

    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * The last error.
     */
    private double error;

    /**
     * The Hessian matrix.
     */
    private final double[][] hessian;

    /**
     * The gradient matrix.
     */
    private final Matrix gradient;

    /**
     * Construct the trainer.
     *
     * @param theAlgorithm    The GLM to train.
     * @param theTrainingData The training data.
     */
    public TrainReweightLeastSquares(final MultipleLinearRegression theAlgorithm, final List<BasicData> theTrainingData) {
        this.algorithm = theAlgorithm;
        this.trainingData = theTrainingData;
        this.gradient = new Matrix(theAlgorithm.getLongTermMemory().length, 1);
        this.hessian = new double[theAlgorithm.getLongTermMemory().length][theAlgorithm.getLongTermMemory().length];
    }

    /**
     * Perform one iteration of training.
     */
    public void iteration() {
        final int rowCount = this.trainingData.size();
        final int coeffCount = this.algorithm.getLongTermMemory().length;

        final double[][] working = new double[rowCount][coeffCount];
        final double[] errors = new double[rowCount];
        final double[] weights = new double[rowCount];
        final Matrix deltas;

        for (int i = 0; i < rowCount; i++) {
            final BasicData element = this.trainingData.get(i);

            working[i][0] = 1;
            for (int j = 0; j < element.getInput().length; j++)
                working[i][j + 1] = element.getInput()[j];
        }

        for (int i = 0; i < rowCount; i++) {
            final BasicData element = this.trainingData.get(i);
            final double y = this.algorithm.computeRegression(element.getInput())[0];
            errors[i] = y - element.getIdeal()[0];
            weights[i] = y * (1.0 - y);
        }

        for (int i = 0; i < gradient.getColumnDimension(); i++) {
            gradient.set(0, i, 0);
            for (int j = 0; j < gradient.getColumnDimension(); j++)
                hessian[i][j] = 0;
        }

        for (int j = 0; j < working.length; j++) {
            for (int i = 0; i < gradient.getRowDimension(); i++) {
                gradient.set(i, 0, gradient.get(i, 0) + working[j][i] * errors[j]);
            }
        }

        for (int k = 0; k < weights.length; k++) {
            final double[] r = working[k];

            for (int j = 0; j < r.length; j++) {
                for (int i = 0; i < r.length; i++) {
                    hessian[j][i] += r[i] * r[j] * weights[k];
                }
            }
        }

        final LUDecomposition lu = new LUDecomposition(new Matrix(hessian));

        if (lu.isNonsingular()) {
            deltas = lu.solve(gradient);
        } else {
            throw new AIFHError("Matrix Non singular");
        }

        final double[] prev = this.algorithm.getLongTermMemory().clone();

        for (int i = 0; i < this.algorithm.getLongTermMemory().length; i++)
            this.algorithm.getLongTermMemory()[i] -= deltas.get(i, 0);

        double max = 0;
        for (int i = 0; i < deltas.getColumnDimension(); i++)
            max = Math.max(Math.abs(deltas.get(i, 0)) / Math.abs(prev[i]), max);

        this.error = max;
    }

    /**
     * @return The last error.
     */
    public double getError() {
        return this.error;
    }
}
