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

public class DeepLayer {
    private double[][] weights;
    private double[] bias;
    private DeepBeliefNetwork owner;
	
	public DeepLayer(DeepBeliefNetwork theOwner, int inputCount, int outputCount) {
		this.weights = new double[outputCount][inputCount];
		this.bias = new double[outputCount];
        this.owner = theOwner;
	}

	public void softmax(double[] x) {
		double max = 0.0;
		double sum = 0.0;

        for (final double aX : x) {
            if (max < aX) {
                max = aX;
            }
        }

		for(int i=0; i<x.length; i++) {
			x[i] = Math.exp(x[i] - max);
			sum += x[i];
		}

		for(int i=0; i<x.length; i++) {
			x[i] /= sum;
		}
	}

    public int getInputCount() {
        return this.weights[0].length;
    }

    public int getOutputCount() {
        return this.weights.length;
    }

    public double[][] getWeights() {
        return this.weights;
    }

    public double[] getBias() {
        return this.bias;
    }

    public DeepBeliefNetwork getOwner() {
        return this.owner;
    }
}