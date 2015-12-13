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
package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public class DropoutLayer extends BasicLayer {

    private double dropoutProbability;
    private final boolean[] active;

    public DropoutLayer(final ActivationFunction theActivation, boolean theHasBias, int theCount, double theDropout) {
        super(theActivation,theHasBias,theCount);
        this.dropoutProbability = theDropout;
        this.active = new boolean[theCount];
        for(int i=0;i<this.active.length;i++) {
            this.active[i] = true;
        }
    }

    @Override
    public void trainingBatch(GenerateRandom rnd) {
        for(int i=0;i<this.active.length;i++) {
            this.active[i] = rnd.nextDouble()>this.dropoutProbability;
        }
    }

    public double getDropoutProbability() {
        return this.dropoutProbability;
    }

    public void setDropoutProbability(double dropoutProbability) {
        this.dropoutProbability = dropoutProbability;
    }

    public boolean[] getActive() {
        return this.active;
    }

    @Override
    public boolean isActive(int i) {
        if( getOwner().isNetworkTraining() ) {
            if (i<this.active.length) {
                return this.active[i];
            } else {
                return true;// bias always active.
            }
        } else {
            return true;
        }
    }
}
