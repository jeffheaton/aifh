package com.heatonresearch.aifh.ann.train;

public interface GradientCalcOwner {

	/**
	 * @return How much to apply l1 regularization penalty, 0 (default) for none.
	 */
	double getL1();

	/**
	 * @return How much to apply l2 regularization penalty, 0 (default) for none.
	 */
	double getL2();

}
