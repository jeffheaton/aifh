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
 * A hidden layer for a DBNN.  This is based on a restricted Boltzmann machine.
 */
public class HiddenLayer extends DeepLayer {

	/**
	 * Create a hidden layer for a DBNN.
	 * @param theOwner The DBNN that this layer belongs to.
	 * @param theInputCount The number of visible units, the input.
	 * @param theOutputCount The number of hidden units, the output.
	 */
    public HiddenLayer(DeepBeliefNetwork theOwner, int theInputCount, int theOutputCount) {
        super(theOwner,theInputCount, theOutputCount);
    }

	/**
	 * Sample n times at probability p and return the count of how many samples were 1 (true).
	 * @param n The number of samples needed.
	 * @param p The probability of choosing 1 (true).
	 * @return The count of how many 1 (true)'s were sampled.
	 */
	public int binomial(int n, double p) {
		if(p < 0 || p > 1) return 0;
		
		int c = 0;
		double r;
		
		for(int i=0; i<n; i++) {
			r = getOwner().getRandom().nextDouble();
			if (r < p) c++;
		}
		
		return c;
	}

	/**
	 * Compute the sigmoid (logisitic) for x.
	 * @param x The value to compute for.
	 * @return The result.
	 */
	public static double sigmoid(double x) {
		return 1.0 / (1.0 + Math.exp(-x));
	}

	/**
	 * Calculate the sigmoid output for this layer.
	 * @param input The input values for this layer's visable.
	 * @param w Thw weights for this layer.
	 * @param b The bias value for this layer.
	 * @return The hidden values for this layer, the output.
	 */
	public double output(double[] input, double[] w, double b) {
		double linearOutput = 0.0;

		// First calculate the linear output.  Similar to linear regression.
		for(int j=0; j<getInputCount(); j++) {
			linearOutput += w[j] * input[j];
		}
		linearOutput += b;

		// Now return the signoid of the linear sum.
		return sigmoid(linearOutput);
	}

	/**
	 * Sample the hidden (h) output values, given the (v) input values.  This is different than the output method
	 * in that we are actually sampling discrete (0 or 1) values.
	 * @param v The visible units.
	 * @param h The hidden units, the count of how many times a true (1) was sampled.
	 */
	public void sampleHgivenV(double[] v, double[] h) {
		for(int i=0; i<getOutputCount(); i++) {
			h[i] = binomial(1, output(v, getWeights()[i], this.getBias()[i]));
		}
	}

	/**
	 * @return The number of input (visible) units.
	 */
    public int getInputCount() {
        return getWeights()[0].length;
    }

	/**
	 * @return The number of output (visible) units.
	 */
    public int getOutputCount() {
        return getWeights().length;
    }
}