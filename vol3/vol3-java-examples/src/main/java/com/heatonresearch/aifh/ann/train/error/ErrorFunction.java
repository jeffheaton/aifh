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
	 * @param af The activation function used at the output layer.
	 * @param ideal The ideal values.
	 * @param actual
	 * @param error
	 */
	void calculateError(ActivationFunction af, double[] b, double[] a,
							   double[] ideal, double[] actual, double[] error, double derivShift,
							   double significance);
}
