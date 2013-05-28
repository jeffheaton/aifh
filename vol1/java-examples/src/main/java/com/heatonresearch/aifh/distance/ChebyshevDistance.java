package com.heatonresearch.aifh.distance;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/28/13
 * Time: 4:35 AM
 * To change this template use File | Settings | File Templates.
 */
public class ChebyshevDistance implements CalculateDistance {
    @Override
    public double calculate(double[] position1, double[] position2) {
        double result = 0;
        for (int i = 0; i < position1.length; i++) {
            double d = Math.abs(position1[i] - position2[i]);
            result = Math.max(d, result);
        }
        return result;
    }
}
