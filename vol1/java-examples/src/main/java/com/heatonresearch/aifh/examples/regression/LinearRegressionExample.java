package com.heatonresearch.aifh.examples.regression;

import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.regression.MultipleLinearRegression;
import com.heatonresearch.aifh.regression.TrainLeastSquares;

import java.io.InputStream;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/1/13
 * Time: 5:08 AM
 * To change this template use File | Settings | File Templates.
 */
public class LinearRegressionExample extends SimpleLearn {
    public void process() {
        try {
            InputStream istream = this.getClass().getResourceAsStream("/abalone.csv");

            DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.encodeOneOfN(0, 0, 1);
            istream.close();

            List<BasicData> trainingData = ds.extractSupervised(0, 10, 10, 1);

            MultipleLinearRegression reg = new MultipleLinearRegression(10);
            TrainLeastSquares train = new TrainLeastSquares(reg, trainingData);
            train.iteration();

            query(reg, trainingData);
            System.out.println("Error: " + train.getError());


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    public static void main(String[] args) {
        LinearRegressionExample prg = new LinearRegressionExample();
        prg.process();
    }
}
