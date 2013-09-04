package com.heatonresearch.aifh.regression;

import Jama.Matrix;
import Jama.QRDecomposition;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;

import java.util.List;

/**
 * Note:, if you get this error message.
 * java.lang.RuntimeException: Matrix is rank deficient.
 * It means that a linear regression cannot be fit to your data.
 */
public class TrainLeastSquares {

    private final MultipleLinearRegression algorithm;
    private final List<BasicData> trainingData;
    private double sst;
    private double sse;
    private final ErrorCalculation errorCalculation = new ErrorCalculationMSE();
    private double error;

    public TrainLeastSquares(MultipleLinearRegression theAlgorithm, List<BasicData> theTrainingData) {
        this.algorithm = theAlgorithm;
        this.trainingData = theTrainingData;
    }

    public double getR2() {
        return 1.0 - this.sse / this.sst;
    }

    public void iteration() {
        int rowCount = trainingData.size();
        int inputColCount = trainingData.get(0).getInput().length;

        Matrix xMatrix = new Matrix(rowCount, inputColCount + 1);
        Matrix yMatrix = new Matrix(rowCount, 1);

        for (int row = 0; row < trainingData.size(); row++) {
            BasicData dataRow = this.trainingData.get(row);
            int colSize = dataRow.getInput().length;

            xMatrix.set(row, 0, 1);
            for (int col = 0; col < colSize; col++) {
                xMatrix.set(row, col + 1, dataRow.getInput()[col]);
            }
            yMatrix.set(row, 0, dataRow.getIdeal()[0]);
        }

        // Calculate the least squares solution
        QRDecomposition qr = new QRDecomposition(xMatrix);
        Matrix beta = qr.solve(yMatrix);

        double sum = 0.0;
        for (int i = 0; i < inputColCount; i++)
            sum += yMatrix.get(i, 0);
        double mean = sum / inputColCount;

        for (int i = 0; i < inputColCount; i++) {
            double dev = yMatrix.get(i, 0) - mean;
            sst += dev * dev;
        }

        Matrix residuals = xMatrix.times(beta).minus(yMatrix);
        sse = residuals.norm2() * residuals.norm2();

        for (int i = 0; i < this.algorithm.getLongTermMemory().length; i++) {
            this.algorithm.getLongTermMemory()[i] = beta.get(i, 0);
        }

        // calculate error
        this.errorCalculation.clear();
        for (BasicData dataRow : this.trainingData) {
            double[] output = this.algorithm.computeRegression(dataRow.getInput());
            this.errorCalculation.updateError(output, dataRow.getIdeal(), 1.0);
        }
        this.error = this.errorCalculation.calculate();
    }

    public double getError() {
        return this.error;
    }
}
