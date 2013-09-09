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

package com.heatonresearch.aifh.discrete;

import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * A simple test subclass for discrete anneal.
 */
public class DiscreteAnnealSubclass extends DiscreteAnneal {
    public static final double[] IDEAL = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
    private final double[] currentHolder = {10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0};
    private final double[] backupHolder = new double[currentHolder.length];
    private final double[] bestHolder = new double[currentHolder.length];
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom(1);
    private final CalculateDistance distance = new EuclideanDistance();

    public DiscreteAnnealSubclass(final int theKMax, final double theStartingTemperature, final double theEndingTemperature) {
        super(theKMax, theStartingTemperature, theEndingTemperature);
        setCycles(1000);
    }

    @Override
    public void backupState() {
        System.arraycopy(this.currentHolder, 0, this.backupHolder, 0, this.currentHolder.length);
    }

    @Override
    public void restoreState() {
        System.arraycopy(this.backupHolder, 0, this.currentHolder, 0, this.currentHolder.length);
    }

    @Override
    public void foundNewBest() {
        System.arraycopy(this.currentHolder, 0, this.bestHolder, 0, this.currentHolder.length);
    }

    @Override
    public void moveToNeighbor() {
        // pick the first point to swap
        final int pt1 = this.rnd.nextInt(this.currentHolder.length);

        // pick the second point to swap, can't be the same as the first
        int pt2;

        do {
            pt2 = this.rnd.nextInt(this.currentHolder.length);
        } while (pt1 == pt2);

        // swap them
        final double temp = this.currentHolder[pt1];
        this.currentHolder[pt1] = this.currentHolder[pt2];
        this.currentHolder[pt2] = temp;
    }

    public double[] getBest() {
        return this.bestHolder;
    }

    @Override
    public double evaluate() {
        return distance.calculate(IDEAL, this.currentHolder);
    }
}
