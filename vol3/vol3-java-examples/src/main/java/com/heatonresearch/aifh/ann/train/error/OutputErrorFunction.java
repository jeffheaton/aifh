/*
 * Encog(tm) Core v3.3 - Java Version
 * http://www.heatonresearch.com/encog/
 * https://github.com/encog/encog-java-core
 
 * Copyright 2008-2014 Heaton Research, Inc.
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
 * A very simple quadratic error function.  It is usually better to use the cross entropy error function.
 */
public class OutputErrorFunction implements ErrorFunction {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void calculateError(ActivationFunction af, double[] b, double[] a,
							   double[] ideal, double[] actual, double[] error, double derivShift,
							   double significance) {
		
		for(int i=0;i<actual.length;i++) {
			double deriv = af.derivativeFunction(b[i],a[i]) + derivShift;
			error[i] = ((ideal[i] - actual[i]) *significance) * deriv;
		}		
	}

}
