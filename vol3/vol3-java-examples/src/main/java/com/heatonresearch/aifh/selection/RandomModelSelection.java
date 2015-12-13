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
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

public class RandomModelSelection extends ModelSelection {

    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    @Override
    public Object[] next() {
        Object[] result = new Object[getSearchAxises().size()];

        for(int i=0;i<result.length;i++) {
            result[i] = getSearchAxises().get(i).sample(this.rnd);
        }

        return result;
    }

    public GenerateRandom getRandom() {
        return this.rnd;
    }

    public void setRandom(GenerateRandom rnd) {
        this.rnd = rnd;
    }
}
