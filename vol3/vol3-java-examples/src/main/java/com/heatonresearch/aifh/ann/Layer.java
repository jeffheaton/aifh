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
import com.heatonresearch.aifh.ann.train.GradientCalc;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * A layer of a BasicNetwork.
 */
public interface Layer {
    /**
     * @return The number of neurons, excluding bias neurons and context neurons. This is the number of neurons that
     * are directly fed from elsewhere.
     */
    int getCount();

    /**
     * @return The number of neurons, including bias neurons and context neurons.
     */
    int getTotalCount();

    /**
     * @return The activation/transfer function for this neuron.
     */
    ActivationFunction getActivation();

    /**
     * Finalize the structure of this layer.
     * @param theOwner The neural network that owns this layer.
     * @param theLayerIndex The zero-based index of this layer.
     * @param counts The counts structure to track the weight and neuron counts.
     */
    void finalizeStructure(BasicNetwork theOwner, int theLayerIndex,
                           TempStructureCounts counts);

    /**
     * Compute this layer.
     */
    void computeLayer();

    /**
     * Compute the gradients for this layer.
     * @param calc The gradient calculation utility.
     */
    void computeGradient(GradientCalc calc);

    /**
     * @return The start of this layer's weights in the weight vector.
     */
    int getWeightIndex();

    /**
     * @return The start of this layer's neurons in the neuron vector.
     */
    int getNeuronIndex();

    /**
     * Notification that a training batch is beginning.
     * @param rnd A random number generator, from the trainer.
     */
    void trainingBatch(GenerateRandom rnd);

    /**
     * @return The owner of the neural network.
     */
    BasicNetwork getOwner();

    /**
     * @return True if this neuron has bias.
     */
    boolean hasBias();
}
