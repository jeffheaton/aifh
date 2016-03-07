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

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * A fully connected weight layer in a neural network.  This layer type is used for input and output layers.
 * This layer type is also one of several hidden layer types available.
 */
public class BasicLayer extends WeightedLayer {
    /**
     * The neuron count.
     */
    private int[] count;

    /**
     * True if this layer has bias.
     */
    private boolean hasBias;


    /**
     * Do not use this constructor.  This was added to support serialization.
     */
    public BasicLayer() {

    }

    /**
     * Construct a multi-dimensional input layer.  This layer is usually used in conjunction with a
     * convolutional neural network (CNN/LeNET).
     * @param theActivation The activation function.
     * @param theHasBias True, if this layer has bias, input layers will usually have bias, others will not.
     * @param theCount The number of neurons in each dimension.
     */
    public BasicLayer(final ActivationFunction theActivation, boolean theHasBias, int[] theCount) {
        if( theCount.length!=1 && theCount.length!=3 ) {
            throw new AIFHError("The number of dimensions must be 1 or 3.");
        }
        setActivation(theActivation);
        this.hasBias = theHasBias;
        this.count = theCount;
    }

    /**
     * Construct a single dimension layer, this is usually used for non-convolutional neural networks.
     * @param theActivation The activation function.  All layers, except input will have activation functions.
     * @param theHasBias True, if this layer has a bias, all layers except the output have bias.
     * @param theCount The neuron count.
     */
    public BasicLayer(final ActivationFunction theActivation, boolean theHasBias, int theCount) {
        this(theActivation,theHasBias,new int[] {theCount});
    }




    /**
     * @return the count
     */
    public int getCount() {
        int product = 1;
        for(int i=0;i<this.count.length;i++) {
            product*=this.count[i];
        }
        return product;
    }

    /**
     * @return The total number of neurons on this layer, includes context, bias
     *         and regular.
     */
    public int getTotalCount() {
        return getCount() + (hasBias() ? 1 : 0);
    }

    /**
     * @return the bias
     */
    public boolean hasBias() {
        return this.hasBias;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void computeLayer() {
        Layer prev = getOwner().getPreviousLayer(this);
        computeLayer(0,0, prev.getTotalCount(), getCount());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void computeGradient(GradientCalc calc) {
        final Layer prev = getOwner().getPreviousLayer(this);
        final int fromLayerSize = prev.getTotalCount();
        final int toLayerSize = getCount();
        this.computeGradient(calc,0,0,fromLayerSize,toLayerSize);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void trainingBatch(GenerateRandom rnd) {
        // Nothing needs to be done!
    }

}
