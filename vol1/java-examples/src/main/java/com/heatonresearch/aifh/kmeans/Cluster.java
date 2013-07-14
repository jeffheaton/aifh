package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.general.data.UnsupervisedData;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A cluster of observations. All observations must have the same number of dimensions.
 */
public class Cluster {

    /**
     * The observations in this cluster.
     */
    private final List<UnsupervisedData> observations = new ArrayList<UnsupervisedData>();

    /**
     * The center of these observations.
     */
    private final double[] center;

    /**
     * Construct a cluster with the specified number of dimensions.
     *
     * @param theDimensions The number of dimensions.
     */
    public Cluster(int theDimensions) {
        this.center = new double[theDimensions];
    }

    /**
     * Get the number of dimensions.
     *
     * @return The number of dimensions.
     */
    public int getDimensions() {
        return this.center.length;
    }

    /**
     * @return The center of the observations.
     */
    public double[] getCenter() {
        return this.center;
    }

    /**
     * @return The observations in this cluster.
     */
    public List<UnsupervisedData> getObservations() {
        return this.observations;
    }

    /**
     * Calculate the center (or mean) of the observations.
     */
    public void calculateCenter() {

        // First, resent the center to zero.
        for (int i = 0; i < center.length; i++) {
            this.center[i] = 0;
        }

        // Now sum up all of the observations to the center.
        for (UnsupervisedData observation : this.observations) {
            for (int i = 0; i < center.length; i++) {
                this.center[i] += observation.getInput()[i];
            }
        }

        // Divide by the number of observations to get the mean.
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
