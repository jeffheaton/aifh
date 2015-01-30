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
package com.heatonresearch.aifh.deep;

public class RestrictedBoltzmannMachine {
    private final double[] hBias;
    private final double[] vBias;
    private final HiddenLayer layer;
    private final DeepBeliefNetwork owner;

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
	
	public static double sigmoid(double x) {
		return 1.0 / (1.0 + Math.exp(-x));
	}
	
	
	public RestrictedBoltzmannMachine(HiddenLayer theLayer) {
        this.layer = theLayer;
        this.owner = theLayer.getOwner();
        this.hBias = layer.getBias();
        this.vBias = new double[getVisibleCount()];
	}

    public int getVisibleCount() {
        return this.layer.getInputCount();
    }

    public int getHiddenCount() {
        return this.layer.getOutputCount();
    }

    public HiddenLayer getLayer() {
        return this.layer;
    }

    public double[] getBiasH() {
        return hBias;
    }

    public double[] getBiasV() {
        return vBias;
    }

    public DeepBeliefNetwork getOwner() {
        return owner;
    }
}