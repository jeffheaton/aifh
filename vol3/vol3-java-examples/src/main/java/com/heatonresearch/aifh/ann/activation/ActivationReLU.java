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
package com.heatonresearch.aifh.ann.activation;

/**
 * A Rectified Linear Unit (ReLU activation function. This activation function is commonly
 * used for hidden layers of a neural network.  A ReLU activation function will usually
 * perform better than tanh and sigmoid.  This is the most popular activation function for
 * deep neural networks.
 *
 * Glorot, X., Bordes, A., & Bengio, Y. (2011). Deep sparse rectifier neural networks. In International Conference
 * on Artificial Intelligence and Statistics (pp. 315-323).
 */
public class ActivationReLU implements ActivationFunction {

	/**
	 * The ramp low threshold parameter.
	 */
	public static final int PARAM_RELU_LOW_THRESHOLD = 0;

	/**
	 * The ramp low parameter.
	 */
	public static final int PARAM_RELU_LOW = 0;


	/**
	 * The parameters.
	 */
	private final double[] params;

	/**
	 * Default constructor.
	 */
	public ActivationReLU() {
		this(0, 0);
	}

	/**
	 * Construct a ramp activation function.
	 * 
	 * @param thresholdLow
	 *            The low threshold value.
	 * @param low
	 *            The low value, replaced if the low threshold is exceeded.
	 */
	public ActivationReLU(final double thresholdLow, final double low) {

		this.params = new double[2];
		this.params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD] = thresholdLow;
		this.params[ActivationReLU.PARAM_RELU_LOW] = low;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void activationFunction(final double[] x, final int start,
			final int size) {
		for (int i = start; i < start + size; i++) {
			if (x[i] <= this.params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD]) {
				x[i] = this.params[ActivationReLU.PARAM_RELU_LOW];
			}
		}

	}

	/**
	 * Clone the object.
	 * 
	 * @return The cloned object.
	 */
	@Override
	public final ActivationFunction clone() {
		return new ActivationReLU(
				this.params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD],
				this.params[ActivationReLU.PARAM_RELU_LOW]);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final double derivativeFunction(final double b, final double a) {
		if(b <= this.params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD])
		{
			return 0;
		}
		return 1.0;
	}

	/**
	 * @return the low
	 */
	public final double getLow() {
		return this.params[ActivationReLU.PARAM_RELU_LOW];
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final String[] getParamNames() {
		final String[] result = {"thresholdLow", "low" };
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final double[] getParams() {
		return this.params;
	}

	/**
	 * @return the thresholdLow
	 */
	public final double getThresholdLow() {
		return this.params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD];
	}

	/**
	 * @return True, as this function does have a derivative.
	 */
	@Override
	public final boolean hasDerivative() {
		return true;
	}

	/**
	 * Set the low value.
	 * 
	 * @param d
	 *            The low value.
	 */
	public final void setLow(final double d) {
		setParam(ActivationReLU.PARAM_RELU_LOW, d);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void setParam(final int index, final double value) {
		this.params[index] = value;
	}

	/**
	 * Set the threshold low.
	 * 
	 * @param d
	 *            The threshold low.
	 */
	public final void setThresholdLow(final double d) {
		setParam(ActivationReLU.PARAM_RELU_LOW_THRESHOLD, d);
	}

}