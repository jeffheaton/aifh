package com.heatonresearch.aifh.distance;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/25/13
 * Time: 4:45 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractDistance implements CalculateDistance {

    @Override
    public double calculate(double[] position1, double[] position2) {
        return calculate(position1, 0, position2, 0, position1.length);
    }
}
