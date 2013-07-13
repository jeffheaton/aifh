package com.heatonresearch.aifh.kmeans;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A cluster of observations.
 */
public class Cluster {

    private final List<double[]> observations = new ArrayList<double[]>();
    private final double[] center;

    public Cluster(int theDimensions) {
        this.center = new double[theDimensions];
    }

    public int getDimensions() {
        return this.center.length;
    }

    public double[] getCenter() {
        return this.center;
    }

    public List<double[]> getObservations() {
        return this.observations;
    }

    public void calculateCenter() {
        for (int i = 0; i < center.length; i++) {
            this.center[i] = 0;
        }

        for (double[] observation : this.observations) {
            for (int i = 0; i < center.length; i++) {
                this.center[i] += observation[i];
            }
        }

        for (int i = 0; i < center.length; i++) {
            this.center[i] /= this.observations.size();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("[Cluster: dimensions=");
        result.append(getDimensions());
        result.append(", observations=");
        result.append(this.observations.size());
        result.append(", center=");
        result.append(Arrays.toString(this.center));
        result.append("]");
        return result.toString();
    }


}
