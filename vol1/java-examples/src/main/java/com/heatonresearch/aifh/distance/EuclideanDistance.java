package com.heatonresearch.aifh.distance;

public class EuclideanDistance extends AbstractDistance {

    @Override
    public double calculate(double[] position1, int pos1, double[] position2, int pos2, int length) {
        double sum = 0;
        for (int i = 0; i < length; i++) {
            double d = position1[i + pos1] - position2[i + pos1];
            sum += d * d;
        }
        return Math.sqrt(sum);
    }
}
