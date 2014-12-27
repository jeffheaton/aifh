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

import Jama.Matrix;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.VectorAlgebra;

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
	 * Train the neural network for the specified pattern. The neural network
	 * can be trained for more than one pattern. To do this simply call the
	 * train method more than once.
	 * 
	 * @param pattern
	 *            The pattern to train for.
	 */
	public void addPattern(final double[] pattern) {

		if (pattern.length != getNeuronCount()) {
			throw new AIFHError("Network with " + getNeuronCount()
					+ " neurons, cannot learn a pattern of size "
					+ pattern.length);
		}

		// Create a row matrix from the input, convert boolean to bipolar
		final Matrix m2 = VectorAlgebra.createRowMatrix(pattern);
		// Transpose the matrix and multiply by the original input matrix
		final Matrix m1 = m2.transpose();
		final Matrix m3 = m1.times(m2);

		// matrix 3 should be square by now, so create an identity
		// matrix of the same size.
		final Matrix identity = VectorAlgebra.identityMatrix(m3.getRowDimension());

		// subtract the identity matrix
		final Matrix m4 = m3.minus(identity);

		// now add the calculated matrix, for this pattern, to the
		// existing weight matrix.
		convertHopfieldMatrix(m4);
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
			result[i] = (getCurrentState()[i]>0)?1:0;
		}
        System.arraycopy(getCurrentState(), 0, result, 0, result.length);
		return result;
	}

	/**
	 * Update the Hopfield weights after training.
	 * 
	 * @param delta
	 *            The amount to change the weights by.
	 */
	private void convertHopfieldMatrix(final Matrix delta) {
		// add the new weight matrix to what is there already
		for (int row = 0; row < delta.getRowDimension(); row++) {
			for (int col = 0; col < delta.getColumnDimension(); col++) {
				addWeight(row, col, delta.get(row, col));
			}
		}
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
			getCurrentState()[toNeuron] = sum;
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
		String lastStateStr = getCurrentState().toString();
		String currentStateStr = getCurrentState().toString();

		int cycle = 0;
		do {
			run();
			cycle++;

			lastStateStr = getCurrentState().toString();

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


}
