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
package com.heatonresearch.aifh.ann.randomize;


import com.heatonresearch.aifh.ann.BasicNetwork;

/**
 * Neural network randomizer that simply assigns each weight and bias to a uniform random number between a high
 * and low range.
 */
public class RangeRandomizeNetwork extends AbstractRandomizeNetwork {

    /**
     * The low end of the range.
     */
    private final double low;

    /**
     * The high end of the range.
     */
    private final double high;

    public RangeRandomizeNetwork(double theLow, double theHigh) {
        this.low = theLow;
        this.high = theHigh;
    }

    /**
     * Create a range randomizer between -1 and 1.
     */
    public RangeRandomizeNetwork() {
        this(-1,1);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void randomize(BasicNetwork network) {
        for(int i=0;i<network.getWeights().length;i++) {
            network.getWeights()[i] = this.getRnd().nextDouble(this.low, this.high);
        }
    }


}
