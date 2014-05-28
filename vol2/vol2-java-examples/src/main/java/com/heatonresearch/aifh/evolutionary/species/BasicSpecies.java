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
package com.heatonresearch.aifh.evolutionary.species;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.population.Population;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Provides basic functionality for a species.
 */
public class BasicSpecies implements Serializable, Species {

    /**
     * Serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The age of this species.
     */
    private int age;

    /**
     * The best score.
     */
    private double bestScore;

    /**
     * The number of generations with no improvement.
     */
    private int gensNoImprovement;

    /**
     * The leader.
     */
    private Genome leader;

    /**
     * The list of genomes.
     */
    private final List<Genome> members = new ArrayList<Genome>();

    /**
     * The owner class.
     */
    private Population population;

    /**
     * The offspring count.
     */
    private transient int offspringCount;

    /**
     * The offpsring share (percent).
     */
    private transient double offspringShare;

    /**
     * Default constructor, used mainly for persistence.
     */
    public BasicSpecies() {

    }

    /**
     * Construct a species.
     *
     * @param thePopulation The population the species belongs to.
     * @param theFirst      The first genome in the species.
     */
    public BasicSpecies(final Population thePopulation, final Genome theFirst) {
        this.population = thePopulation;
        this.bestScore = theFirst.getScore();
        this.gensNoImprovement = 0;
        this.age = 0;
        this.leader = theFirst;
        this.members.add(theFirst);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void add(final Genome genome) {
        genome.setPopulation(this.population);
        this.members.add(genome);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateShare(final boolean shouldMinimize,
                                 final double maxScore) {
        double total = 0;

        int count = 0;
        for (final Genome genome : this.members) {
            if (!Double.isNaN(genome.getAdjustedScore())
                    && !Double.isInfinite(genome.getAdjustedScore())) {
                double s;
                if (shouldMinimize) {
                    s = maxScore - genome.getAdjustedScore();
                } else {
                    s = genome.getAdjustedScore();
                }
                total += s;
                count++;
            }
        }

        if (count == 0) {
            this.offspringShare = 0;
        } else {
            this.offspringShare = total / count;
        }

        return this.offspringShare;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getAge() {
        return this.age;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getBestScore() {
        return this.bestScore;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getGensNoImprovement() {
        return this.gensNoImprovement;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Genome getLeader() {
        return this.leader;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Genome> getMembers() {
        return this.members;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getOffspringCount() {
        return this.offspringCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getOffspringShare() {
        return this.offspringShare;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Population getPopulation() {
        return this.population;
    }

    /**
     * Purge all members, increase age by one and count the number of
     * generations with no improvement.
     */
    public void purge() {
        this.members.clear();
        if (this.leader != null) {
            this.members.add(this.leader);
        }
        this.age++;
        this.gensNoImprovement++;
        this.offspringCount = 0;
        this.offspringShare = 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setAge(final int theAge) {
        this.age = theAge;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setBestScore(final double theBestScore) {
        this.bestScore = theBestScore;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setGensNoImprovement(final int theGensNoImprovement) {
        this.gensNoImprovement = theGensNoImprovement;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLeader(final Genome theLeader) {
        this.leader = theLeader;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setOffspringCount(final int offspringCount) {
        this.offspringCount = offspringCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setPopulation(final Population thePopulation) {
        this.population = thePopulation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder result = new StringBuilder();
        result.append("[BasicSpecies: score=");
        result.append(getBestScore());
        result.append(", members=");
        result.append(this.members.size());
        result.append(", age=");
        result.append(this.age);
        result.append(", no_improv=");
        result.append(this.gensNoImprovement);
        result.append(", share=");
        result.append(this.offspringShare);
        result.append(", offspring count=");
        result.append(this.offspringShare);
        result.append("]");
        return result.toString();
    }

}
