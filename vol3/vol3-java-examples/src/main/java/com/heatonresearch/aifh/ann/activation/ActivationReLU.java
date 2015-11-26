package com.heatonresearch.aifh.ann.activation;

import org.encog.ml.factory.MLActivationFactory;
import org.encog.util.obj.ActivationUtil;

/**
 * A ramp activation function. This function has a high and low threshold. If
 * the high threshold is exceeded a fixed value is returned. Likewise, if the
 * low value is exceeded another fixed value is returned.
 * 
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
	 * The serial ID.
	 */
	private static final long serialVersionUID = 6336245112244386279L;

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