package com.heatonresearch.aifh.distance;

public class ChebyshevDistance extends AbstractDistance {

    @Override
    public double calculate(double[] position1, int pos1, double[] position2, int pos2, int length) {
        double result = 0;
        for (int i = 0; i < length; i++) {
            double d = Math.abs(position1[pos1 + i] - position2[pos2 + i]);
            result = Math.max(d, result);
        }
        return result;
    }
}
