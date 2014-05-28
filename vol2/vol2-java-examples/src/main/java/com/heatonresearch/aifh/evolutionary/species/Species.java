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

import java.util.List;

/**
 * Defines a species.
 */
public interface Species {

    /**
     * Add a genome to this species.
     *
     * @param genome The genome to add.
     */
    void add(Genome genome);

    /**
     * Calculate this genome's share of the next population.
     *
     * @param shouldMinimize True if we see to minimize the score.
     * @param maxScore       The best score.
     * @return The share of this species, as a percent ratio.
     */
    double calculateShare(boolean shouldMinimize, double maxScore);

    /**
     * @return The age of this species.
     */
    int getAge();

    /**
     * @return The best score for this species.
     */
    double getBestScore();

    /**
     * @return The number of generations with no imrpvement.
     */
    int getGensNoImprovement();

    /**
     * @return The leader of this species.
     */
    Genome getLeader();

    /**
     * @return The members of this species.
     */
    List<Genome> getMembers();

    /**
     * @return Get the offspring count.
     */
    int getOffspringCount();

    /**
     * @return The offspring share for the next iteration's population.
     */
    double getOffspringShare();

    /**
     * @return The population.
     */
    Population getPopulation();

    /**
     * Set the age of this species.
     *
     * @param theAge The age.
     */
    void setAge(int theAge);

    /**
     * Set the best score for this species.
     *
     * @param theBestScore The best score.
     */
    void setBestScore(double theBestScore);

    /**
     * Set the number of generations with no improvement.
     *
     * @param theGensNoImprovement The generation count with no improvement.
     */
    void setGensNoImprovement(int theGensNoImprovement);

    /**
     * Set the leader of this species.
     *
     * @param theLeader The leader.
     */
    void setLeader(Genome theLeader);

    /**
     * Set the offspring count.
     *
     * @param offspringCount The offspring count.
     */
    void setOffspringCount(int offspringCount);

    /**
     * Set the population.
     *
     * @param thePopulation The population.
     */
    void setPopulation(Population thePopulation);
}
