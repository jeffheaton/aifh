package com.heatonresearch.aifh.ann.train.error;


import com.heatonresearch.aifh.ann.activation.ActivationFunction;

/**
 * Implements a cross entropy error function.  This can be used with backpropagation to
 * sometimes provide better performance than the standard linear error function.
 * @author jheaton
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
