package com.heatonresearch.aifh.ann.train;

public interface GradientCalcOwner {
	/**
	 * Called by the worker threads to report the progress at each step.
	 * 
	 * @param gradients
	 *            The gradients from that worker.
	 * @param error
	 *            The error for that worker.
	 * @param ex
	 *            The exception.
	 */
	public void report(final double[] gradients, final double error,
			final Throwable ex);
	
	/**
	 * @return How much to apply l1 regularization penalty, 0 (default) for none.
	 */
	public double getL1();

	/**
	 * @return How much to apply l2 regularization penalty, 0 (default) for none.
	 */
	public double getL2();

}
