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
package com.heatonresearch.aifh.energetic;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * The energetic network forms the base class for Hopfield and Boltzmann machines.
 * @author jheaton
 *
 */
public abstract class EnergeticNetwork implements MLMethod {

	/**
	 * Serial id.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The current state of the thermal network.
	 */
	private double[] currentState;

	/**
	 * The weights.
	 */
	private double[] weights;

	/**
	 * The neuron count.
	 */
	private int neuronCount;

	/**
	 * Default constructor.
	 */
	public EnergeticNetwork() {

	}

	/**
	 * Construct the network with the specified neuron count.
	 * @param neuronCount The number of neurons.
	 */
	public EnergeticNetwork(final int neuronCount) {
		this.neuronCount = neuronCount;
		this.weights = new double[neuronCount * neuronCount];
		this.currentState = new double[neuronCount];
	}

	/**
	 * Add to the specified weight.
	 * @param fromNeuron The from neuron.
	 * @param toNeuron The to neuron.
	 * @param value The value to add.
	 */
	public void addWeight(final int fromNeuron, final int toNeuron,
			final double value) {
		final int index = (toNeuron * this.neuronCount) + fromNeuron;
		if (index >= this.weights.length) {
			throw new AIFHError("Out of range: fromNeuron:"
					+ fromNeuron + ", toNeuron: " + toNeuron);
		}
		this.weights[index] += value;
	}

	/**
	 * @return Calculate the current energy for the network. The network will
	 *         seek to lower this value.
	 */
	public double calculateEnergy() {
		double tempE = 0;
		final int neuronCount = getNeuronCount();

		for (int i = 0; i < neuronCount; i++) {
			for (int j = 0; j < neuronCount; j++) {
				if (i != j) {
					tempE += getWeight(i, j) * this.currentState[i]
							* this.currentState[j];
				}
			}
		}
		return -1 * tempE / 2;

	}

	/**
	 * Clear any connection weights.
	 */
	public void clear() {
        for(int i=0;i<this.weights.length;i++) {
            this.weights[i] = 0;
        }
	}

	/**
	 * @return The current state of the network.
	 */
	public double[] getCurrentState() {
		return this.currentState;
	}

	/**
	 * @return Get the neuron count for the network.
	 */
	public int getNeuronCount() {
		return this.neuronCount;
	}

	/**
	 * Get a weight.
	 * @param fromNeuron The from neuron.
	 * @param toNeuron The to neuron.
	 * @return The weight.
	 */
	public double getWeight(final int fromNeuron, final int toNeuron) {
		final int index = (toNeuron * this.neuronCount) + fromNeuron;
		return this.weights[index];
	}

	/**
	 * @return The weights.
	 */
	public double[] getWeights() {
		return this.weights;
	}

	/**
	 * Init the network.
	 * @param neuronCount The neuron count.
	 * @param weights The weights.
	 * @param output The output.
	 */
	public void init(final int neuronCount, final double[] weights,
			final double[] output) {
		if (neuronCount != output.length) {
			throw new AIFHError("Neuron count(" + neuronCount
					+ ") must match output count(" + output.length + ").");
		}

		if ((neuronCount * neuronCount) != weights.length) {
			throw new AIFHError("Weight count(" + weights.length
					+ ") must be the square of the neuron count(" + neuronCount
					+ ").");
		}

		this.neuronCount = neuronCount;
		this.weights = weights;
		this.currentState = new double[neuronCount];
		System.arraycopy(output, 0, this.currentState, 0, this.currentState.length);
	}

	/**
	 * Randomize the weights.
	 * @param rand The random number generator to use.
     */
	public void reset(final GenerateRandom rand) {

        for(int i=0;i<this.currentState.length;i++) {
            this.currentState[i] = 0;
        }
        for(int i=0;i<this.weights.length;i++) {
            this.weights[i] = 0;
        }
	}


	/**
	 * Set the current state.
	 * @param s The current state array.
	 */
	public void setCurrentState(final double[] s) {
		this.currentState = new double[s.length];
        System.arraycopy(s, 0, this.currentState, 0, s.length);
	}

	/**
	 * Set the neuron count.
	 * @param c The neuron count.
	 */
	public void setNeuronCount(final int c) {
		this.neuronCount = c;

	}

	/**
	 * Set the weight.
	 * @param fromNeuron The from neuron.
	 * @param toNeuron The to neuron.
	 * @param value The value.
	 */
	public void setWeight(final int fromNeuron, final int toNeuron,
			final double value) {
		final int index = (toNeuron * this.neuronCount) + fromNeuron;
		this.weights[index] = value;
	}

	/**
	 * Set the weight array.
	 * @param w The weight array.
	 */
	public void setWeights(final double[] w) {
		this.weights = w;
	}



    @Override
    public double[] getLongTermMemory() {
        return this.weights;
    }
}
