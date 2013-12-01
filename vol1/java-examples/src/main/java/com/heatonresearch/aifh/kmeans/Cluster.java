/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.general.data.BasicData;

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
    private final List<BasicData> observations = new ArrayList<BasicData>();

    /**
     * The center of these observations.
     */
    private final double[] center;

    /**
     * Construct a cluster with the specified number of dimensions.
     *
     * @param theDimensions The number of dimensions.
     */
    public Cluster(final int theDimensions) {
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
    public List<BasicData> getObservations() {
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
        for (final BasicData observation : this.observations) {
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
        final StringBuilder result = new StringBuilder();
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
