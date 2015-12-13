/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
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
package com.heatonresearch.aifh.selection;

import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * A numeric range search axis.
 */
public class NumericSearchAxis implements SearchAxis {
    /**
     * The start of the range.
     */
    private final double start;

    /**
     * The end of the range.
     */
    private final double stop;

    /**
     * The step in the range.
     */
    private final double step;

    /**
     * The current position in the range.
     */
    private double currentState;

    /**
     * Construct a numeric range axis.
     * @param start The start of the range.
     * @param stop Where to stop in the range.
     * @param step The step for the range.
     */
    public NumericSearchAxis(double start, double stop, double step) {
        this.start = start;
        this.stop = stop;
        this.step = step;
    }

    /**
     * @return The start of the range.
     */
    public double getStart() {
        return this.start;
    }

    /**
     * @return The end of the range.
     */
    public double getStop() {
        return this.stop;
    }

    /**
     * @return The step value for the range.
     */
    public double getStep() {
        return this.step;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void reset() {
        this.currentState = this.start;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean advance() {
        this.currentState+=this.step;
        if( this.currentState>=this.stop) {
            this.currentState=this.start;
            return true;
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object currentState() {
        return new Double(this.currentState);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object sample(GenerateRandom rnd) {
        return rnd.nextDouble(this.start,this.stop);
    }
}
