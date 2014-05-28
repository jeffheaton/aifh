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
package com.heatonresearch.aifh.evolutionary.genome;

import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.Species;

import java.io.Serializable;

/**
 * A basic abstract genome. Provides base functionality.
 */
public abstract class BasicGenome implements Genome, Serializable {

    /**
     * Serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The adjusted score. If unknown, it is set to NaN.
     */
    private double adjustedScore = Double.NaN;

    /**
     * The score of this genome.
     */
    private double score = Double.NaN;

    /**
     * The population this genome belongs to.
     */
    private Population population;

    /**
     * The birth generation for this genome.
     */
    private int birthGeneration;

    /**
     * The species of this genome.
     */
    private Species species;

    /**
     * @return The adjusted score, which considers bonuses.
     */
    @Override
    public double getAdjustedScore() {
        return this.adjustedScore;
    }

    /**
     * @return the birthGeneration
     */
    @Override
    public int getBirthGeneration() {
        return this.birthGeneration;
    }

    /**
     * @return the population
     */
    @Override
    public Population getPopulation() {
        return this.population;
    }

    /**
     * @return The score.
     */
    @Override
    public double getScore() {
        return this.score;
    }

    /**
     * Set the adjusted score.
     *
     * @param theAdjustedScore The score.
     */
    @Override
    public void setAdjustedScore(final double theAdjustedScore) {
        this.adjustedScore = theAdjustedScore;
    }

    /**
     * @param birthGeneration the birthGeneration to set
     */
    @Override
    public void setBirthGeneration(final int birthGeneration) {
        this.birthGeneration = birthGeneration;
    }

    /**
     * @param thePopulation the population to set
     */
    @Override
    public void setPopulation(final Population thePopulation) {
        this.population = thePopulation;
    }

    /**
     * Set the score.
     *
     * @param theScore Set the score.
     */
    @Override
    public void setScore(final double theScore) {
        this.score = theScore;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append("[");
        builder.append(this.getClass().getSimpleName());
        builder.append(": score=");
        builder.append(getScore());
        return builder.toString();
    }

    /**
     * @return the species
     */
    @Override
    public Species getSpecies() {
        return species;
    }

    /**
     * @param s the species to set
     */
    @Override
    public void setSpecies(Species s) {
        this.species = s;
    }

}
