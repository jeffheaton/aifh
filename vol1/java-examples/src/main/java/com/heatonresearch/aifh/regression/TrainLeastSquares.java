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

import Jama.Matrix;
import Jama.QRDecomposition;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationSSE;
import com.heatonresearch.aifh.general.data.BasicData;

import java.util.List;

/**
 * Train a Linear Regression with Least Squares.  This will only work if you use the default identity function.
 * <p/>
 * <p/>
 * Note:, if you get this error message.
 * java.lang.RuntimeException: Matrix is rank deficient.
 * It means that a linear regression cannot be fit to your data.
 */
public class TrainLeastSquares {
    /**
     * The linear regression object we are training.
     */
    private final MultipleLinearRegression algorithm;

    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * Total sum of squares.
     */
    private double sst;

    /**
     * Sum of squares for error.
     */
    private double sse;

    /**
     * An error calculation method.
     */
    private final ErrorCalculation errorCalculation = new ErrorCalculationSSE();

    /**
     * The last error.
     */
    private double error;

    /**
     * Construct the trainer.
     *
     * @param theAlgorithm    The algorithm to train.
     * @param theTrainingData The training data.
     */
    public TrainLeastSquares(final MultipleLinearRegression theAlgorithm, final List<BasicData> theTrainingData) {
        this.algorithm = theAlgorithm;
        this.trainingData = theTrainingData;
    }

    /**
     * @return The R squared value.  The coefficient of determination.
     */
    public double getR2() {
        return 1.0 - this.sse / this.sst;
    }

    /**
     * Train.  Single iteration.
     */
    public void iteration() {
        final int rowCount = trainingData.size();
        final int inputColCount = trainingData.get(0).getInput().length;

        final Matrix xMatrix = new Matrix(rowCount, inputColCount + 1);
        final Matrix yMatrix = new Matrix(rowCount, 1);

        for (int row = 0; row < trainingData.size(); row++) {
            final BasicData dataRow = this.trainingData.get(row);
            final int colSize = dataRow.getInput().length;

            xMatrix.set(row, 0, 1);
            for (int col = 0; col < colSize; col++) {
                xMatrix.set(row, col + 1, dataRow.getInput()[col]);
            }
            yMatrix.set(row, 0, dataRow.getIdeal()[0]);
        }

        // Calculate the least squares solution
        final QRDecomposition qr = new QRDecomposition(xMatrix);
        final Matrix beta = qr.solve(yMatrix);

        double sum = 0.0;
        for (int i = 0; i < inputColCount; i++)
            sum += yMatrix.get(i, 0);
        final double mean = sum / inputColCount;

        for (int i = 0; i < inputColCount; i++) {
            final double dev = yMatrix.get(i, 0) - mean;
            sst += dev * dev;
        }

        final Matrix residuals = xMatrix.times(beta).minus(yMatrix);
        sse = residuals.norm2() * residuals.norm2();

        for (int i = 0; i < this.algorithm.getLongTermMemory().length; i++) {
            this.algorithm.getLongTermMemory()[i] = beta.get(i, 0);
        }

        // calculate error
        this.errorCalculation.clear();
        for (final BasicData dataRow : this.trainingData) {
            final double[] output = this.algorithm.computeRegression(dataRow.getInput());
            this.errorCalculation.updateError(output, dataRow.getIdeal(), 1.0);
        }
        this.error = this.errorCalculation.calculate();
    }

    /**
     * @return The current error.
     */
    public double getError() {
        return this.error;
    }
}
