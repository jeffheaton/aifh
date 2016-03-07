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
package com.heatonresearch.aifh.ann.train.error;


import com.heatonresearch.aifh.ann.activation.ActivationFunction;

/**
 * Implements a cross entropy error function.  This can be used with backpropagation to
 * sometimes provide better performance than the standard linear error function.
 *
 * De Boer, Pieter-Tjerk, et al. "A tutorial on the cross-entropy method." Annals of operations
 * research 134.1 (2005): 19-67.
 *
 */
public class CrossEntropyErrorFunction implements ErrorFunction {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void calculateError(ActivationFunction af, double[] b, double[] a,
							   double[] ideal, double[] actual, double[] error, double derivShift,
							   double significance) {
		
		for(int i=0;i<actual.length;i++) {
			error[i] = (ideal[i] - actual[i]) *significance;
		}		
	}

}
