/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.aco;

/**
 * An individual ant for continuous ACO.
 */
public class ContinuousAnt implements Comparable<ContinuousAnt> {

    /**
     * The score for this ant.
     */
    private double score;

    /**
     * The parameters for this ant.
     */
    private final double[] params;

    /**
     * True, if this ant should minimize.  This value should be the same for all ants.
     */
    private boolean shouldMinimize;

    /**
     * The constructor.
     *
     * @param n                 The number of parameters (dimensions).
     * @param theShouldMinimize True, if we are minimizing.
     */
    public ContinuousAnt(int n, boolean theShouldMinimize) {
        this.params = new double[n];
        this.shouldMinimize = theShouldMinimize;
    }

    /**
     * @return The score for this ant.
     */
    public double getScore() {
        return score;
    }

    /**
     * Set the score for this ant.
     *
     * @param score The score.
     */
    public void setScore(final double score) {
        this.score = score;
    }

    /**
     * @return The parameters for this ant.
     */
    public double[] getParams() {
        return params;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(final ContinuousAnt o) {
        if (shouldMinimize) {
            return Double.compare(getScore(), o.getScore());
        } else {
            return Double.compare(o.getScore(), getScore());
        }
    }
}
