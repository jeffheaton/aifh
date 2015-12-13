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
package com.heatonresearch.aifh.general.collections;

import java.io.Serializable;

/**
 * Holds an object of the specified type.  Used with the ObjectChooser.
 * Associates an object with a probability.
 *
 * @param <T> The type of object to hold.
 */
public class ObjectHolder<T> implements Serializable {
    /**
     * The object to hold.
     */
    private final T obj;

    /**
     * The probability.
     */
    private final double probability;

    public ObjectHolder(T theObj, double probability) {
        this.obj = theObj;
        this.probability = probability;
    }

    /**
     * @return the opp
     */
    public T getObj() {
        return this.obj;
    }

    /**
     * @return the probability
     */
    public double getProbability() {
        return this.probability;
    }
}
