package com.heatonresearch.aifh.distance;

public interface CalculateDistance {
    double calculate(double[] position1, double[] position2);

    double calculate(double[] position1, int pos1, double[] position2, int pos2, int length);
}
