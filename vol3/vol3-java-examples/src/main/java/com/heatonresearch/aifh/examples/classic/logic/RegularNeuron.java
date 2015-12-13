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
package com.heatonresearch.aifh.examples.classic.logic;

import java.util.ArrayList;
import java.util.List;

/**
 * A regular neuron, can function as either a hidden our output.
 */
public class RegularNeuron implements Neuron {
    /**
     * The parents to this neuron.
     */
    private final List<Connection> parents = new ArrayList<>();

    /**
     * The bias.
     */
    private final double bias;

    /**
     * Construct the neuron.
     * @param bias The neuron's bias.
     */
    public RegularNeuron(double bias) {
        this.bias = bias;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double compute() {
        double sum = this.bias;
        for(Connection c : this.parents) {
            sum += c.getWeight() * c.getParent().compute();
        }

        if(sum>=0.5) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * @return The parent neurons.
     */
    public List<Connection> getParents() {
        return this.parents;
    }
}
