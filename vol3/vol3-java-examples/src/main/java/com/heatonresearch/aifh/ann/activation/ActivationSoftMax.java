package com.heatonresearch.aifh.ann.activation;

import org.encog.Encog;
import org.encog.mathutil.BoundMath;

/**
 * The softmax activation function.
 * 
 * @author jheaton
 */
public class ActivationSoftMax implements ActivationFunction {

	/**
	 * The serial id.
	 */
	private static final long serialVersionUID = -960489243250457611L;

	/**
	 * The parameters.
	 */
	private final double[] params;

	/**
	 * Construct the soft-max activation function.
	 */
	public ActivationSoftMax() {
		this.params = new double[0];
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void activationFunction(final double[] x, final int start,
			final int size) {
		double sum = 0;
		for (int i = start; i < start + size; i++) {
			x[i] = BoundMath.exp(x[i]);
			sum += x[i];
		}
		if(Double.isNaN(sum) || sum <Encog.DEFAULT_DOUBLE_EQUAL ) {
			for (int i = start; i < start + size; i++) {
				x[i] = 1.0/size;
			}
		} else {
			for (int i = start; i < start + size; i++) {
				x[i] = x[i] / sum;
			}
		}
	}

	/**
	 * @return The object cloned;
	 */
	@Override
	public final ActivationFunction clone() {
		return new ActivationSoftMax();
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
	 * @return Return false, softmax has no derivative.
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
