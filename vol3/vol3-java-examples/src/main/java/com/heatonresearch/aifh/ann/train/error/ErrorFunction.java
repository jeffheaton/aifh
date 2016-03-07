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
 * An error function.  This is used to calculate the errors for the
 * output layer during propagation training.
 *
 */
public interface ErrorFunction {
    /**
     * Calculate the error.
     * @param af The activation function.
     * @param b The output, before the activation function.
     * @param a The output, after the activation function.
     * @param ideal The idea/expected output.
     * @param actual The actual output.
     * @param error Error vector (output)
     * @param derivShift Any derivative shift to apply (usually 0.0), used to implement flat-spot problem shift.
     * @param significance The significance weight (usually 1.0)
     */
	void calculateError(ActivationFunction af, double[] b, double[] a,
						double[] ideal, double[] actual, double[] error, double derivShift,
						double significance);
}
