package com.heatonresearch.aifh.ann.activation;

/**
 * The hyperbolic tangent activation function takes the curved shape of the
 * hyperbolic tangent. This activation function produces both positive and
 * negative output. Use this activation function if both negative and positive
 * output is desired.
 * 
 */
public class ActivationTANH implements ActivationFunction {

	/**
	 * Serial id for this class.
	 */
	private static final long serialVersionUID = 9121998892720207643L;

	/**
	 * The parameters.
	 */
	private final double[] params;

	/**
	 * Construct a basic HTAN activation function, with a slope of 1.
	 */
	public ActivationTANH() {
		this.params = new double[0];
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void activationFunction(final double[] x, final int start,
			final int size) {
		for (int i = start; i < start + size; i++) {
			x[i] = Math.tanh(x[i]);
		}
	}

	/**
	 * @return The object cloned;
	 */
	@Override
	public final ActivationFunction clone() {
		return new ActivationTANH();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final double derivativeFunction(final double b, final double a) {
		return (1.0 - a * a);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final String[] getParamNames() {
		final String[] result = {};
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
	 * @return Return true, TANH has a derivative.
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
