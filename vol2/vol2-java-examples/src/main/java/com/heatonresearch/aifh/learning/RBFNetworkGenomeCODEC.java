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
package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/21/14
 * Time: 5:52 AM
 * To change this template use File | Settings | File Templates.
 */
public class RBFNetworkGenomeCODEC implements GeneticCODEC {

    public int getInputCount() {
        return inputCount;
    }

    private int inputCount;

    public int getOutputCount() {
        return outputCount;
    }

    public int getRbfCount() {
        return rbfCount;
    }

    public int size() {
        return size;
    }

    private int outputCount;
    private int rbfCount;
    private int size;

    public RBFNetworkGenomeCODEC(int inputCount, int rbfCount, int outputCount) {
        this.inputCount = inputCount;
        this.rbfCount = rbfCount;
        this.outputCount = outputCount;
        RBFNetwork temp = new RBFNetwork(inputCount, rbfCount, outputCount);
        this.size = temp.getLongTermMemory().length;
    }

    @Override
    public MLMethod decode(final Genome genome) {
        RBFNetwork result = new RBFNetwork(inputCount, rbfCount, outputCount);
        DoubleArrayGenome dag = (DoubleArrayGenome) genome;
        System.arraycopy(dag.getData(), 0, result.getLongTermMemory(), 0, size);
        return result;
    }

    @Override
    public Genome encode(final MLMethod phenotype) {
        RBFNetwork rbfNet = (RBFNetwork) phenotype;
        DoubleArrayGenome result = new DoubleArrayGenome(size());
        System.arraycopy(rbfNet.getLongTermMemory(), 0, result.getData(), 0, size);
        return result;
    }
}
