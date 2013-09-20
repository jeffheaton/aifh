/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * The Nelder-Mead method is a commonly used parameter optimization method that
 * can be used for machine learning. It typically provides a good error
 * rate and is relatively fast.
 * <p/>
 * Nelder-Mead must build a simplex, which is an n*(n+1) matrix of weights. If
 * you have a large number of weights, this matrix can quickly overflow memory.
 * <p/>
 * The biggest enhancement that is needed for this trainer is to make use of
 * multi-threaded code to evaluate the speed evaluations when training on a
 * multi-core.
 * <p/>
 * This implementation is based on the source code provided by John Burkardt
 * (http://people.sc.fsu.edu/~jburkardt/)
 * <p/>
 * http://people.sc.fsu.edu/~jburkardt/c_src/asa047/asa047.c
 */
public class TrainNelderMead implements LearningMethod {

    /**
     * True if the network has converged, and no further training is needed.
     */
    private boolean converged = false;

    /**
     * Used to calculate the centroid.
     */
    public final static double CCOEFF = 0.5;
    public final static double ECOEFF = 2.0;
    public final static double EPS = 0.001;
    public final static double RCOEFF = 1.0;

    private double del;

    private int jcount;
    private final int nn;
    private final double[] p;
    private final double[] p2star;
    private final double[] pbar;
    private final double[] pstar;
    private final double rq;
    private final double[] y;
    private final double[] start;
    private final double[] trainedWeights;
    private final double[] step;
    private final int konvge;
    private final MachineLearningAlgorithm algorithm;
    private final ScoreFunction score;
    private double lastError;

    public TrainNelderMead(final MachineLearningAlgorithm theAlgorithm, final ScoreFunction theScore) {
        this(theAlgorithm, theScore, 100);
    }

    public TrainNelderMead(final MachineLearningAlgorithm theAlgorithm, final ScoreFunction theScore, final double stepValue) {
        this.algorithm = theAlgorithm;
        this.score = theScore;

        this.start = this.algorithm.getLongTermMemory().clone();
        this.trainedWeights = this.algorithm.getLongTermMemory().clone();

        final int n = this.start.length;

        this.p = new double[n * (n + 1)];
        this.pstar = new double[n];
        this.p2star = new double[n];
        this.pbar = new double[n];
        this.y = new double[n + 1];

        this.nn = n + 1;
        this.del = 1.0;
        this.rq = 0.000001 * n;

        this.step = new double[this.start.length];
        this.jcount = this.konvge = 500;
        for (int i = 0; i < this.step.length; i++) {
            this.step[i] = stepValue;
        }
    }

    /**
     * Calculate the error for the neural network with a given set of weights.
     *
     * @param weights The weights to use.
     * @return The current error.
     */
    public double fn(final double[] weights) {
        System.arraycopy(weights, 0, this.algorithm.getLongTermMemory(), 0, weights.length);
        return score.calculateScore(this.algorithm);

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean done() {
        return this.converged;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStatus() {
        return "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {

        if (this.converged) {
            return;
        }

        final int n = this.start.length;

        System.arraycopy(this.start, 0, this.p, n * n, n);
        this.y[n] = fn(this.start);
        for (int j = 0; j < n; j++) {
            final double x = this.start[j];
            this.start[j] = this.start[j] + this.step[j] * this.del;
            System.arraycopy(this.start, 0, this.p, j * n, n);
            this.y[j] = fn(this.start);
            this.start[j] = x;
        }
        /*
         * The simplex construction is complete.
		 *
		 * Find highest and lowest Y values. YNEWLO = Y(IHI) indicates the
		 * vertex of the simplex to be replaced.
		 */
        double ylo = this.y[0];
        int ilo = 0;

        for (int i = 1; i < this.nn; i++) {
            if (this.y[i] < ylo) {
                ylo = this.y[i];
                ilo = i;
            }
        }
        /*
         * Inner loop.
		 */
        double ynewlo;

        double z;
        for (; ; ) {
            /*
             * if (kcount <= icount) { break; }
			 */
            ynewlo = this.y[0];
            int ihi = 0;

            for (int i = 1; i < this.nn; i++) {
                if (ynewlo < this.y[i]) {
                    ynewlo = this.y[i];
                    ihi = i;
                }
            }
            /*
             * Calculate PBAR, the centroid of the simplex vertices excepting
			 * the vertex with Y value YNEWLO.
			 */
            for (int i = 0; i < n; i++) {
                z = 0.0;
                for (int j = 0; j < this.nn; j++) {
                    z = z + this.p[i + j * n];
                }
                z = z - this.p[i + ihi * n];
                this.pbar[i] = z / n;
            }
            /*
             * Reflection through the centroid.
			 */
            for (int i = 0; i < n; i++) {
                this.pstar[i] = this.pbar[i] + RCOEFF
                        * (this.pbar[i] - this.p[i + ihi * n]);
            }
            final double ystar = fn(this.pstar);
            /*
             * Successful reflection, so extension.
			 */
            final double y2star;
            if (ystar < ylo) {
                for (int i = 0; i < n; i++) {
                    this.p2star[i] = this.pbar[i] + ECOEFF
                            * (this.pstar[i] - this.pbar[i]);
                }
                y2star = fn(this.p2star);
				/*
				 * Check extension.
				 */
                if (ystar < y2star) {
                    System.arraycopy(this.pstar, 0, this.p, ihi * n, n);
                    this.y[ihi] = ystar;
                }
				/*
				 * Retain extension or contraction.
				 */
                else {
                    System.arraycopy(this.p2star, 0, this.p, ihi * n, n);
                    this.y[ihi] = y2star;
                }
            }
			/*
			 * No extension.
			 */
            else {
                int l = 0;
                for (int i = 0; i < this.nn; i++) {
                    if (ystar < this.y[i]) {
                        l = l + 1;
                    }
                }

                if (1 < l) {
                    System.arraycopy(this.pstar, 0, this.p, ihi * n, n);
                    this.y[ihi] = ystar;
                }
				/*
				 * Contraction on the Y(IHI) side of the centroid.
				 */
                else if (l == 0) {
                    for (int i = 0; i < n; i++) {
                        this.p2star[i] = this.pbar[i] + CCOEFF
                                * (this.p[i + ihi * n] - this.pbar[i]);
                    }
                    y2star = fn(this.p2star);
					/*
					 * Contract the whole simplex.
					 */
                    if (this.y[ihi] < y2star) {
                        for (int j = 0; j < this.nn; j++) {
                            for (int i = 0; i < n; i++) {
                                this.p[i + j * n] = (this.p[i + j * n] + this.p[i
                                        + ilo * n]) * 0.5;
                                this.trainedWeights[i] = this.p[i + j * n];
                            }
                            this.y[j] = fn(this.trainedWeights);
                        }
                        ylo = this.y[0];
                        ilo = 0;

                        for (int i = 1; i < this.nn; i++) {
                            if (this.y[i] < ylo) {
                                ylo = this.y[i];
                                ilo = i;
                            }
                        }
                        continue;
                    }
					/*
					 * Retain contraction.
					 */
                    else {
                        System.arraycopy(this.p2star, 0, this.p, ihi * n, n);
                        this.y[ihi] = y2star;
                    }
                }
				/*
				 * Contraction on the reflection side of the centroid.
				 */
                else if (l == 1) {
                    for (int i = 0; i < n; i++) {
                        this.p2star[i] = this.pbar[i] + CCOEFF
                                * (this.pstar[i] - this.pbar[i]);
                    }
                    y2star = fn(this.p2star);
					/*
					 * Retain reflection?
					 */
                    if (y2star <= ystar) {
                        System.arraycopy(this.p2star, 0, this.p, ihi * n, n);
                        this.y[ihi] = y2star;
                    } else {
                        System.arraycopy(this.pstar, 0, this.p, ihi * n, n);
                        this.y[ihi] = ystar;
                    }
                }
            }
			/*
			 * Check if YLO improved.
			 */
            if (this.y[ihi] < ylo) {
                ylo = this.y[ihi];
                ilo = ihi;
            }
            this.jcount = this.jcount - 1;

            if (0 < this.jcount) {
                continue;
            }
			/*
			 * Check to see if minimum reached.
			 */
            // if (icount <= kcount)
            {
                this.jcount = this.konvge;

                z = 0.0;
                for (int i = 0; i < this.nn; i++) {
                    z = z + this.y[i];
                }
                final double x = z / this.nn;

                z = 0.0;
                for (int i = 0; i < this.nn; i++) {
                    z = z + Math.pow(this.y[i] - x, 2);
                }

                if (z <= this.rq) {
                    break;
                }
            }
        }
		/*
		 * Factorial tests to check that YNEWLO is a local minimum.
		 */
        System.arraycopy(this.p, ilo * n, this.trainedWeights, 0, n);
        ynewlo = this.y[ilo];

        boolean fault = false;

        for (int i = 0; i < n; i++) {
            this.del = this.step[i] * EPS;
            this.trainedWeights[i] += this.del;
            z = fn(this.trainedWeights);
            if (z < ynewlo) {
                fault = true;
                break;
            }
            this.trainedWeights[i] = this.trainedWeights[i] - this.del
                    - this.del;
            z = fn(this.trainedWeights);
            if (z < ynewlo) {
                fault = true;
                break;
            }
            this.trainedWeights[i] += this.del;
        }

        if (!fault) {
            this.converged = true;
        } else {
			/*
			 * Restart the procedure.
			 */
            System.arraycopy(this.trainedWeights, 0, this.start, 0, n);
            this.del = EPS;
        }

        this.lastError = ynewlo;
        System.arraycopy(this.trainedWeights, 0, this.algorithm.getLongTermMemory(), 0, this.trainedWeights.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getLastError() {
        return this.lastError;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finishTraining() {

    }
}
