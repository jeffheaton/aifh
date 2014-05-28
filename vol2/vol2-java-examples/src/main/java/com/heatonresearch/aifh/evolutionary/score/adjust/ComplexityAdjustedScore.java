/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.evolutionary.score.adjust;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.score.AdjustScore;

/**
 * Adjust scores to penalize complexity.
 */
public class ComplexityAdjustedScore implements AdjustScore {

    /**
     * The starting complexity penalty.
     */
    private double complexityPenalty = .2;

    /**
     * The full complexity penalty.
     */
    private double complexityFullPenalty = 2.0;

    /**
     * The complexity level at which a penalty begins to be applied.
     */
    private int complexityPenaltyThreshold = 10;

    /**
     * The complexity level at which a full (100%) penalty is applied.
     */
    private int complexityPentaltyFullThreshold = 50;

    /**
     * Construct a adjustor to penalize complexity.
     *
     * @param theComplexityPenaltyThreshold The complexity level at which a penalty begins to be applied.
     * @param theComplexityPentaltyFullThreshold
     *                                      The complexity level at which a full (100%) penalty is applied.
     * @param theComplexityPenalty          The starting complexity penalty.
     * @param theComplexityFullPenalty      The full complexity penalty.
     */
    public ComplexityAdjustedScore(int theComplexityPenaltyThreshold,
                                   int theComplexityPentaltyFullThreshold,
                                   double theComplexityPenalty, double theComplexityFullPenalty) {
        this.complexityPenaltyThreshold = theComplexityPenaltyThreshold;
        this.complexityPentaltyFullThreshold = theComplexityPentaltyFullThreshold;
        this.complexityPenalty = theComplexityPenalty;
        this.complexityFullPenalty = theComplexityFullPenalty;
    }

    public ComplexityAdjustedScore() {
        this(10, 50, 0.2, 2.0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateAdjustment(Genome genome) {
        double score = genome.getScore();
        double result = 0;

        if (genome.size() > this.complexityPenaltyThreshold) {
            int over = genome.size() - this.complexityPenaltyThreshold;
            int range = this.complexityPentaltyFullThreshold
                    - this.complexityPenaltyThreshold;
            double complexityPenalty = ((this.complexityFullPenalty - this.complexityPenalty) / range)
                    * over;
            result = (score * complexityPenalty);
        }

        return result;
    }

    /**
     * @return the complexityPenalty
     */
    public double getComplexityPenalty() {
        return complexityPenalty;
    }

    /**
     * @param complexityPenalty the complexityPenalty to set
     */
    public void setComplexityPenalty(double complexityPenalty) {
        this.complexityPenalty = complexityPenalty;
    }

    /**
     * @return the complexityFullPenalty
     */
    public double getComplexityFullPenalty() {
        return complexityFullPenalty;
    }

    /**
     * @param complexityFullPenalty the complexityFullPenalty to set
     */
    public void setComplexityFullPenalty(double complexityFullPenalty) {
        this.complexityFullPenalty = complexityFullPenalty;
    }

    /**
     * @return the complexityPenaltyThreshold
     */
    public int getComplexityPenaltyThreshold() {
        return complexityPenaltyThreshold;
    }

    /**
     * @param complexityPenaltyThreshold the complexityPenaltyThreshold to set
     */
    public void setComplexityPenaltyThreshold(int complexityPenaltyThreshold) {
        this.complexityPenaltyThreshold = complexityPenaltyThreshold;
    }

    /**
     * @return the complexityPentaltyFullThreshold
     */
    public int getComplexityPentaltyFullThreshold() {
        return complexityPentaltyFullThreshold;
    }

    /**
     * @param complexityPentaltyFullThreshold
     *         the complexityPentaltyFullThreshold to set
     */
    public void setComplexityPentaltyFullThreshold(
            int complexityPentaltyFullThreshold) {
        this.complexityPentaltyFullThreshold = complexityPentaltyFullThreshold;
    }


}
