package com.heatonresearch.aifh.ann.activation;

/**
 * The Linear layer is really not an activation function at all. The input is
 * simply passed on, unmodified, to the output. This activation function is
 * primarily theoretical and of little actual use. Usually an activation
 * function that scales between 0 and 1 or -1 and 1 should be used.
 */
public class ActivationLinear implements ActivationFunction {

	/**
	 * Default empty parameters.
	 */
	public static final double P[] = new double[0];
	
	/**
	 * Default empty parameters.
	 */
	public static final String N[] = new String[0];
	
	/**
	 * Serial id for this class.
	 */
	private static final long serialVersionUID = -5356580554235104944L;

	/**
	 * The parameters.
	 */
	private final double[] params;

	/**
	 * Construct a linear activation function, with a slope of 1.
	 */
	public ActivationLinear() {
		this.params = new double[0];
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void activationFunction(final double[] x, final int start,
			final int size) {
	}

	/**
	 * @return The object cloned.
	 */
	@Override
	public final ActivationFunction clone() {
		return new ActivationLinear();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final double derivativeFunction(final double b, final double a) {
		return 1;
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
	 * @return Return true, linear has a 1 derivative.
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
