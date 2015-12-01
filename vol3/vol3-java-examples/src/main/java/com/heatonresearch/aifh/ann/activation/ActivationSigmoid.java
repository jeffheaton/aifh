package com.heatonresearch.aifh.ann.activation;

import org.encog.mathutil.BoundMath;

/**
 * The sigmoid activation function takes on a sigmoidal shape. Only positive
 * numbers are generated. Do not use this activation function if negative number
 * output is desired.
 */
public class ActivationSigmoid implements ActivationFunction {

	/**
	 * Serial id for this class.
	 */
	private static final long serialVersionUID = 5622349801036468572L;

	/**
	 * The parameters.
	 */
	private final double[] params;

	/**
	 * Construct a basic sigmoid function, with a slope of 1.
	 */
	public ActivationSigmoid() {
		this.params = new double[0];
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void activationFunction(final double[] x, final int start,
			final int size) {
		for (int i = start; i < start + size; i++) {
			x[i] = 1.0 / (1.0 + BoundMath.exp(-1 * x[i]));
		}
	}

	/**
	 * @return The object cloned;
	 */
	@Override
	public final ActivationFunction clone() {
		return new ActivationSigmoid();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final double derivativeFunction(final double b, final double a) {
		return a * (1.0 - a);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final String[] getParamNames() {
		final String[] results = {};
		return results;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final double[] getParams() {
		return this.params;
	}

	/**
	 * @return True, sigmoid has a derivative.
	 */
	@Override
	public final boolean hasDerivative() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void setParam(final int index, final double value) {
		this.params[index] = value;
	}

}
