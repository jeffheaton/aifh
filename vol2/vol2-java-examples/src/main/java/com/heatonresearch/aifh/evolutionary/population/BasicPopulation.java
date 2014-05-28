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
package com.heatonresearch.aifh.evolutionary.population;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.genome.GenomeFactory;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.species.Species;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Defines the basic functionality for a population of genomes. The population
 * is made up of species. These species contain the individiual genomes that
 * make up the population. If you do not want to use species, then create one
 * species that holds every genome.
 */
public class BasicPopulation implements Population,
        Serializable {

    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The object name.
     */
    private String name;

    /**
     * The species that make up the population.
     */
    private final List<Species> species = new ArrayList<Species>();

    /**
     * The best genome.
     */
    private Genome bestGenome;

    /**
     * A factory that can be used to store create genomes.
     */
    private GenomeFactory genomeFactory;

    /**
     * How many genomes should be created.
     */
    private int populationSize;

    /**
     * Construct an empty population.
     */
    public BasicPopulation() {
        this.populationSize = 0;
    }

    /**
     * Construct a population.
     *
     * @param thePopulationSize The population size.
     */
    public BasicPopulation(final int thePopulationSize,
                           final GenomeFactory theGenomeFactory) {
        this.populationSize = thePopulationSize;
        this.genomeFactory = theGenomeFactory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        this.species.clear();

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Species createSpecies() {
        final Species species = new BasicSpecies();
        species.setPopulation(this);
        getSpecies().add(species);
        return species;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Species determineBestSpecies() {
        for (final Species species : this.species) {
            if (species.getMembers().contains(this.bestGenome)) {
                return species;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Genome> flatten() {
        final List<Genome> result = new ArrayList<Genome>();
        for (final Species species : this.species) {
            result.addAll(species.getMembers());
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Genome getBestGenome() {
        return this.bestGenome;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GenomeFactory getGenomeFactory() {
        return this.genomeFactory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxIndividualSize() {
        return Integer.MAX_VALUE;
    }

    /**
     * @return The name.
     */
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getPopulationSize() {
        return this.populationSize;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Species> getSpecies() {
        return this.species;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setBestGenome(final Genome genome) {
        this.bestGenome = genome;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setGenomeFactory(final GenomeFactory factory) {
        this.genomeFactory = factory;
    }

    /**
     * Set the name.
     *
     * @param theName The new name.
     */
    public void setName(final String theName) {
        this.name = theName;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setPopulationSize(final int thePopulationSize) {
        this.populationSize = thePopulationSize;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return flatten().size();
    }

    /**
     * Not supported, a population cannot be fit by methods that use getLongTermMemory.
     * The population is made up of many elements.
     *
     * @return Nothing.
     */
    @Override
    public double[] getLongTermMemory() {
        throw new UnsupportedOperationException();
    }

    /**
     * Purge any invalid genomes.
     */
    public void purgeInvalidGenomes() {
        // remove any invalid genomes
        int speciesNum = 0;
        while (speciesNum < getSpecies().size()) {
            Species species = getSpecies().get(speciesNum);

            int genomeNum = 0;
            while (genomeNum < species.getMembers().size()) {
                Genome genome = species.getMembers().get(genomeNum);
                if (Double.isInfinite(genome.getScore())
                        || Double.isInfinite(genome.getAdjustedScore())
                        || Double.isNaN(genome.getScore())
                        || Double.isNaN(genome.getAdjustedScore())) {
                    species.getMembers().remove(genome);
                } else {
                    genomeNum++;
                }
            }

            // is the species now empty?
            if (species.getMembers().size() == 0) {
                getSpecies().remove(species);
            } else {
                // new leader needed?
                if (!species.getMembers().contains(species.getLeader())) {
                    species.setLeader(species.getMembers().get(0));
                    species.setBestScore(species.getLeader().getScore());
                }

                // onto the next one!
                speciesNum++;
            }
        }
    }
}
