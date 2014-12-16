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
package com.heatonresearch.aifh.randomize;

/**
 * Provides the ability for subclasses to generate normally distributed random numbers.
 */
public abstract class AbstractBoxMuller extends AbstractGenerateRandom {

    /**
     * The y2 value.
     */
    private double y2;

    /**
     * Should we use the last value.
     */
    private boolean useLast = false;

    /**
     * The mean.
     */
    public static final double MU = 0;

    /**
     * The standard deviation.
     */
    private static final double SIGMA = 1;


    /**
     * {@inheritDoc}
     */
    @Override
    public double nextGaussian() {
        double x1;
        double x2;
        double w;
        final double y1;

        // use value from previous call
        if (this.useLast) {
            y1 = this.y2;
            this.useLast = false;
        } else {
            do {
                x1 = 2.0 * nextDouble() - 1.0;
                x2 = 2.0 * nextDouble() - 1.0;
                w = x1 * x1 + x2 * x2;
            } while (w >= 1.0);

            w = Math.sqrt((-2.0 * Math.log(w)) / w);
            y1 = x1 * w;
            this.y2 = x2 * w;
            this.useLast = true;
        }

        return (AbstractBoxMuller.MU + y1 * AbstractBoxMuller.SIGMA);
    }
}
