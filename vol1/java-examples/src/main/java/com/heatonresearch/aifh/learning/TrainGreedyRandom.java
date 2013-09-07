package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * The Greedy Random learning algorithm is a very primitive random-walk algorithm that only takes steps that serve
 * to move the Machine Learning algorithm to a more optimal position.  This learning algorithm essentially chooses
 * random locations for the long term memory until a better set is found.
 * <p/>
 * http://en.wikipedia.org/wiki/Random_walk
 */
public class TrainGreedyRandom implements LearningAlgorithm {
    /**
     * The Machine Learning algorithm to optimize.
     */
    private final MachineLearningAlgorithm algorithm;

    /**
     * The random number generator to use.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * The last error.
     */
    private double lastError;

    /**
     * The score function.
     */
    private final ScoreFunction score;

    /**
     * The low range for random number selection.
     */
    private double lowRange = -10;

    /**
     * The high range for random number selection.
     */
    private double highRange = 10;

    /**
     * True, if we are minimizing the score function.
     */
    private final boolean shouldMinimize;

    /**
     * Construct a greedy random algorithm.
     *
     * @param theShouldMinimize True, if we should minimize.
     * @param theAlgorithm      The algorithm to optimize.
     * @param theScore          The score function.
     */
    public TrainGreedyRandom(boolean theShouldMinimize, MachineLearningAlgorithm theAlgorithm, ScoreFunction theScore) {
        this.algorithm = theAlgorithm;
        this.score = theScore;
        this.shouldMinimize = theShouldMinimize;

        // Set the last error to a really bad value so it will be reset on the first iteration.
        if (this.shouldMinimize) {
            this.lastError = Double.POSITIVE_INFINITY;
        } else {
            this.lastError = Double.NEGATIVE_INFINITY;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        int len = this.algorithm.getLongTermMemory().length;

        // backup current state
        double[] oldState = new double[len];
        System.arraycopy(this.algorithm.getLongTermMemory(), 0, oldState, 0, len);

        // randomize the method
        performRandomize(this.algorithm.getLongTermMemory());

        // did we improve it?  Only keep the new method if it improved (greedy).
        double currentError = score.calculateScore(this.algorithm);

        if ((currentError < this.lastError) ? shouldMinimize : !shouldMinimize) {
            this.lastError = currentError;
        } else {
            System.arraycopy(oldState, 0, this.algorithm.getLongTermMemory(), 0, len);
        }
    }

    /**
     * Randomly move to a new location.  To specify a new randomization function, override this method.
     *
     * @param memory The long term memory.
     */
    public void performRandomize(double[] memory) {
        for (int i = 0; i < memory.length; i++) {
            memory[i] = this.rnd.nextDouble(this.lowRange, this.highRange);
        }
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
    public double getLastError() {
        return this.lastError;
    }

    /**
     * @return The low range.
     */
    public double getLowRange() {
        return lowRange;
    }

    /**
     * Set the low range.
     *
     * @param lowRange The low range.
     */
    public void setLowRange(final double lowRange) {
        this.lowRange = lowRange;
    }

    /**
     * @return The high range.
     */
    public double getHighRange() {
        return highRange;
    }

    /**
     * Set the high range.
     *
     * @param highRange The high range.
     */
    public void setHighRange(final double highRange) {
        this.highRange = highRange;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean done() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finishTraining() {

    }
}
