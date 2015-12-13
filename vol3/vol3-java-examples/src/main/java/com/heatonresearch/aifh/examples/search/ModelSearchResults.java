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
package com.heatonresearch.aifh.examples.search;

import java.util.Arrays;

public class ModelSearchResults implements Comparable<ModelSearchResults> {
    private final int epochs;
    private final double error;
    private final Object[] hyperParameters;

    public ModelSearchResults(int epochs, double error, Object[] hyperParameters) {
        this.epochs = epochs;
        this.error = error;
        this.hyperParameters = hyperParameters;
    }

    public int getEpochs() {
        return this.epochs;
    }

    public double getError() {
        return this.error;
    }

    @Override
    public String toString() {
        return Arrays.toString(this.hyperParameters) + " : error=" + this.error + ", epocs=" + this.epochs;
    }

    @Override
    public int compareTo(ModelSearchResults o) {
        return Double.compare(this.error,o.getError());
    }
}
