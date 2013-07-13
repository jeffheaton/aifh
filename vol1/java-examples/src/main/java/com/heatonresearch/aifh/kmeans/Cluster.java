package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.general.data.UnsupervisedData;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A cluster of observations.
 */
public class Cluster {

    private final List<UnsupervisedData> observations = new ArrayList<UnsupervisedData>();
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

    public List<UnsupervisedData> getObservations() {
        return this.observations;
    }

    public void calculateCenter() {
        for (int i = 0; i < center.length; i++) {
            this.center[i] = 0;
        }

        for (UnsupervisedData observation : this.observations) {
            for (int i = 0; i < center.length; i++) {
                this.center[i] += observation.getInput()[i];
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
