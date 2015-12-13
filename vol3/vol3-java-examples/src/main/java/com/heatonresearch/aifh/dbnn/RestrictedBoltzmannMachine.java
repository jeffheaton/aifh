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
package com.heatonresearch.aifh.dbnn;

/**
 * Restricted Boltzmann machine, for deep belief neural network.
 */
public class RestrictedBoltzmannMachine {

    /**
     * The hidden bias.
     */
    private final double[] hBias;

    /**
     * The visable bias.
     */
    private final double[] vBias;

    /**
     * The hidden layer that this RBM corresponds to.
     */
    private final HiddenLayer layer;

    /**
     * The neural network.
     */
    private final DeepBeliefNetwork owner;

    /**
     * Sample a bimodal value with the specified probability.  Returns the count of sampled true values.
     * @param n The number of values to sample.
     * @param p The probability of true.
     * @return The count of true values.
     */
	public int binomial(int n, double p) {
		if(p < 0 || p > 1) return 0;
		
		int c = 0;
		double r;
		
		for(int i=0; i<n; i++) {
			r = this.owner.getRandom().nextDouble();
			if (r < p) c++;
		}
		
		return c;
	}

    /**
     * Sigmoid function.
     * @param x The input.
     * @return The output.
     */
	public static double sigmoid(double x) {
		return 1.0 / (1.0 + Math.exp(-x));
	}

    /**
     * Construct restricted Boltzmann machine.
     * @param theLayer The layer that this RBM works with.
     */
	public RestrictedBoltzmannMachine(HiddenLayer theLayer) {
        this.layer = theLayer;
        this.owner = theLayer.getOwner();
        this.hBias = this.layer.getBias();
        this.vBias = new double[getVisibleCount()];
	}

    /**
     * @return The visable neuron count.
     */
    public int getVisibleCount() {
        return this.layer.getInputCount();
    }

    /**
     * @return The hidden neuron count.
     */
    public int getHiddenCount() {
        return this.layer.getOutputCount();
    }

    /**
     * @return The hidden layer that goes with this RBM.
     */
    public HiddenLayer getLayer() {
        return this.layer;
    }

    /**
     * @return Hidden biases.
     */
    public double[] getBiasH() {
        return this.hBias;
    }

    /**
     * @return Visable biases.
     */
    public double[] getBiasV() {
        return this.vBias;
    }

    /**
     * @return The network owner.
     */
    public DeepBeliefNetwork getOwner() {
        return this.owner;
    }
}