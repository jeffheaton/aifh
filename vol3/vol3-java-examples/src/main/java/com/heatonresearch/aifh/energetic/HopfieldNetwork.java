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

import java.util.Arrays;

/**
 * Implements a Hopfield network.
 * 
 */
public class HopfieldNetwork extends EnergeticNetwork {

	/**
	 * Serial id.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Default constructor.
	 */
	public HopfieldNetwork() {

	}

	/**
	 * Construct a Hopfield with the specified neuron count.
	 * @param neuronCount The neuron count.
	 */
	public HopfieldNetwork(final int neuronCount) {
		super(neuronCount);
	}

	/**
	 * Note: for Hopfield networks, you will usually want to call the "run"
	 * method to compute the output.
	 * 
	 * This method can be used to copy the input data to the current state. A
	 * single iteration is then run, and the new current state is returned.
	 * 
	 * @param input
	 *            The input pattern.
	 * @return The new current state.
	 */
	public double[] compute(final double[] input) {
		final double[] result = new double[input.length];
        System.arraycopy(input, 0, getCurrentState(), 0, input.length);
		run();

		for (int i = 0; i < getCurrentState().length; i++) {
			result[i] = activationFunction(getCurrentState()[i]);
		}
        System.arraycopy(getCurrentState(), 0, result, 0, result.length);
		return result;
	}

    public double activationFunction(double d) {
        return (d>0)?1:0;
    }

	/**
	 * {@inheritDoc}
	 */
	public int getInputCount() {
		return super.getNeuronCount();
	}

	/**
	 * {@inheritDoc}
	 */
	public int getOutputCount() {
		return super.getNeuronCount();
	}

	/**
	 * Perform one Hopfield iteration.
	 */
	public void run() {

		for (int toNeuron = 0; toNeuron < getNeuronCount(); toNeuron++) {
			double sum = 0;
			for (int fromNeuron = 0; fromNeuron < getNeuronCount(); fromNeuron++) {
				sum += getCurrentState()[fromNeuron]
						* getWeight(fromNeuron, toNeuron);
			}
			getCurrentState()[toNeuron] = activationFunction(sum);
        }
	}

	/**
	 * Run the network until it becomes stable and does not change from more
	 * runs.
	 * 
	 * @param max
	 *            The maximum number of cycles to run before giving up.
	 * @return The number of cycles that were run.
	 */
	public int runUntilStable(final int max) {
		boolean done = false;
		String lastStateStr = Arrays.toString(getCurrentState());
		String currentStateStr = lastStateStr;

		int cycle = 0;
		do {
			run();
			cycle++;

			lastStateStr = Arrays.toString(getCurrentState());

			if (!currentStateStr.equals(lastStateStr)) {
				if (cycle > max) {
					done = true;
				}
			} else {
				done = true;
			}

			currentStateStr = lastStateStr;

		} while (!done);

		return cycle;
	}

	/**
	 * Calculate the energy for this network.
	 * @return The energy.
     */
    public double energy() {
        double t = 0;

        // Calculate first term
        double a = 0;
        for(int i=0;i<this.getInputCount();i++) {
            for(int j=0;j<this.getOutputCount();j++) {
                a+=this.getWeight(i,j) * this.getCurrentState()[i] * this.getCurrentState()[j];
            }
        }
        a*=-0.5;

        // Calculate second term
        double b = 0;
        for(int i=0;i<this.getInputCount();i++) {
            b+=this.getCurrentState()[i] * t;
        }

        return a+b;
    }

}
