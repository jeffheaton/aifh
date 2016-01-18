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
import com.heatonresearch.aifh.flat.FlatMatrix;
import com.heatonresearch.aifh.flat.FlatObject;

/**
 * Base class for all layers (used with BasicNetwork) that have weights.
 */
public abstract class WeightedLayer implements Layer {

    /**
     * The layer index.
     */
    private int layerIndex;

    /**
     * The network that owns this layer.
     */
    private BasicNetwork owner;

    /**
     * The index to this layer's weights.
     */
    private int weightIndex;

    /**
     * The index to this layer's neurons.
     */
    private int neuronIndex;

    /**
     * The activation function.
     */
    private ActivationFunction activation;

    /**
     * The layer's weight matrix.
     */
    private FlatMatrix weightMatrix;

    /**
     * {@inheritDoc}
     */
    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex) {
        this.owner = theOwner;
        this.layerIndex = theLayerIndex;

        Layer prevLayer = (this.layerIndex>0) ? this.owner.getLayers().get(this.layerIndex-1) : null;

        if( prevLayer!=null ) {
            this.weightMatrix = new FlatMatrix(prevLayer.getTotalCount(), getCount());
        }
    }

    /**
     * Compute a layer.
     */
    @Override
    public void computeLayer() {
        Layer prev = getOwner().getPreviousLayer(this);

        // weight values
        for (int ix = 0; ix < getCount(); ix++) {
            double sum = 0;

            for (int y = 0; y < prev.getTotalCount(); y++) {
                if(prev.isActive(ix) && isActive(y)) {
                    sum += this.weightMatrix.get(ix,y) * prev.getLayerOutput().get(y);
                }
            }
            getLayerSums().add(ix, sum);
            getLayerOutput().add(ix, sum);
        }

        getActivation().activationFunction(
                this.owner.getLayerOutput().getData(),getLayerOutput().getOffset(), getCount());
    }

    /**
     * Compute gradients for this layer.
     * @param calc The gradient calculator.
     */
    @Override
    public void computeGradient(GradientCalc calc) {
        final Layer prev = getOwner().getPreviousLayer(this);
        final FlatObject prevLayerDelta = calc.getLayerDelta().get(getLayerIndex()-1);
        final FlatObject layerDelta = calc.getLayerDelta().get(getLayerIndex());

        final ActivationFunction activation = getActivation();
        FlatMatrix gradientMatrix = (FlatMatrix)calc.getGradientMatrix().getFlatObjects().get(2-getLayerIndex());


        for (int yi = 0; yi < prev.getTotalCount(); yi++) {
            final double output = prev.getLayerOutput().get(yi);
            double sum = 0;

            for (int xi = 0; xi < getCount(); xi++) {

                if (prev.isActive(yi) && isActive(xi))
                    gradientMatrix.add(xi,yi, -(output * layerDelta.get(xi)));
                sum += this.weightMatrix.get(yi,xi) * layerDelta.get(xi);
            }
            prevLayerDelta.set(yi, sum
                    * (activation.derivativeFunction(prev.getLayerSums().get(yi), prev.getLayerOutput().get(yi))));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLayerIndex() { return this.layerIndex; }

    /**
     * {@inheritDoc}
     */
    @Override
    public BasicNetwork getOwner() {
        return this.owner;
    }

    /**
     * @param activation
     *            the activation to set
     */
    public void setActivation(final ActivationFunction activation) {
        this.activation = activation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ActivationFunction getActivation() {
        return this.activation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(",count=").append(getCount());

        result.append("]");
        return result.toString();
    }

    public FlatMatrix getWeightMatrix() {
        return weightMatrix;
    }
}
