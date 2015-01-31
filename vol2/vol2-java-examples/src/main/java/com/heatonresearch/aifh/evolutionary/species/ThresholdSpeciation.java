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

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.sort.SortGenomesForSpecies;
import com.heatonresearch.aifh.evolutionary.sort.SpeciesComparator;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Speciate based on threshold. Any genomes with a compatibility score below a
 * level will be in the same species.
 */
public abstract class ThresholdSpeciation implements Speciation, Serializable {
    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The training being used.
     */
    private EvolutionaryAlgorithm owner;

    /**
     * The minimum compatibility that two genes must have to be in the same
     * species.
     */
    private double compatibilityThreshold = 1.0;

    /**
     * The maximum number of generations allows with no improvement. After this
     * the genomes in this species are not allowed to reproduce or continue.
     * This does not apply to top species.
     */
    private int numGensAllowedNoImprovement = 15;

    /**
     * The maximum number of species. This is just a target. If the number of
     * species goes over this number then the compatibilityThreshold is
     * increased to decrease the number of species.
     */
    private int maxNumberOfSpecies = 40;

    /**
     * The method used to sort the genomes in the species. More desirable
     * genomes should come first for later selection.
     */
    private SortGenomesForSpecies sortGenomes;

    /**
     * The population.
     */
    private Population population;

    /**
     * Add a genome.
     *
     * @param species The species to add to.
     * @param genome  The genome to add.
     */
    public void addSpeciesMember(final Species species, final Genome genome) {

        if (this.owner.isValidationMode()) {
            if (species.getMembers().contains(genome)) {
                throw new AIFHError("Species already contains genome: "
                        + genome.toString());
            }
        }

        if (this.owner.getSelectionComparator().compare(genome,
                species.getLeader()) < 0) {
            species.setBestScore(genome.getAdjustedScore());
            species.setGensNoImprovement(0);
            species.setLeader(genome);
        }

        species.add(genome);
    }

    /**
     * Adjust the species compatibility threshold. This prevents us from having
     * too many species. Dynamically increase or decrease the
     * compatibilityThreshold.
     */
    private void adjustCompatibilityThreshold() {

        // has this been disabled (unlimited species)
        if (this.maxNumberOfSpecies < 1) {
            return;
        }

        final double thresholdIncrement = 0.01;

        if (this.population.getSpecies().size() > this.maxNumberOfSpecies) {
            this.compatibilityThreshold += thresholdIncrement;
        } else if (this.population.getSpecies().size() < 2) {
            this.compatibilityThreshold -= thresholdIncrement;
        }
    }

    /**
     * Divide up the potential offspring by the most fit species. To do this we
     * look at the total species score, vs each individual species percent
     * contribution to that score.
     *
     * @param speciesCollection The current species list.
     * @param totalSpeciesScore The total score over all species.
     */
    private void divideByFittestSpecies(final List<Species> speciesCollection,
                                        final double totalSpeciesScore) {
        Species bestSpecies = findBestSpecies();

        // loop over all species and calculate its share
        final Object[] speciesArray = speciesCollection.toArray();
        for (final Object element : speciesArray) {
            final Species species = (Species) element;
            // calculate the species share based on the percent of the total
            // species score
            int share = (int) Math
                    .round((species.getOffspringShare() / totalSpeciesScore)
                            * this.owner.getPopulation().getPopulationSize());

            // do not give the best species a zero-share
            if ((species == bestSpecies) && (share == 0)) {
                share = 1;
            }

            // if the share is zero, then remove the species
            if ((species.getMembers().size() == 0) || (share == 0)) {
                removeSpecies(species);
            }
            // if the species has not improved over the specified number of
            // generations, then remove it.
            else if ((species.getGensNoImprovement() > this.numGensAllowedNoImprovement)
                    && (species != bestSpecies)) {
                removeSpecies(species);
            } else {
                // otherwise assign a share and sort the members.
                species.setOffspringCount(share);
                Collections.sort(species.getMembers(), this.sortGenomes);
            }
        }
    }

    /**
     * Find the best species.
     *
     * @return The best species.
     */
    public Species findBestSpecies() {
        if (this.owner.getBestGenome() != null) {
            return this.owner.getBestGenome().getSpecies();
        }
        return null;
    }

    /**
     * Attempt to remove a removable species. If the species is the best
     * species, then do not remove it. If the species is the last species, don't
     * remove it.
     *
     * @param species The species to attempt to remove.
     */
    public void removeSpecies(Species species) {
        if (species != findBestSpecies()) {
            if (population.getSpecies().size() > 1) {
                this.population.getSpecies().remove(species);
            }
        }
    }

    /**
     * If no species has a good score then divide the potential offspring amount
     * all species evenly.
     *
     * @param speciesCollection The current set of species.
     */
    private void divideEven(final List<Species> speciesCollection) {
        final double ratio = 1.0 / speciesCollection.size();
        for (final Species species : speciesCollection) {
            final int share = (int) Math.round(ratio
                    * this.owner.getPopulation().getPopulationSize());
            species.setOffspringCount(share);
        }
    }

    /**
     * @return the compatibilityThreshold
     */
    public double getCompatibilityThreshold() {
        return this.compatibilityThreshold;
    }

    /**
     * @return the maxNumberOfSpecies
     */
    public int getMaxNumberOfSpecies() {
        return this.maxNumberOfSpecies;
    }

    /**
     * @return the numGensAllowedNoImprovement
     */
    public int getNumGensAllowedNoImprovement() {
        return this.numGensAllowedNoImprovement;
    }

    /**
     * @return the owner
     */
    public EvolutionaryAlgorithm getOwner() {
        return this.owner;
    }

    /**
     * @return the sortGenomes
     */
    public SortGenomesForSpecies getSortGenomes() {
        return this.sortGenomes;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init(final EvolutionaryAlgorithm theOwner) {
        this.owner = theOwner;
        this.population = theOwner.getPopulation();
        this.sortGenomes = new SortGenomesForSpecies(this.owner);
    }

    /**
     * Level off all of the species shares so that they add up to the desired
     * population size. If they do not add up to the desired species size, this
     * was a result of rounding the floating point share amounts to integers.
     */
    private void levelOff() {
        int total = 0;
        final List<Species> list = this.population.getSpecies();

        if (list.size() == 0) {
            throw new AIFHError(
                    "Can't speciate, next generation contains no species.");
        }

        Collections.sort(list, new SpeciesComparator(this.owner));

        // best species gets at least one offspring
        if (list.get(0).getOffspringCount() == 0) {
            list.get(0).setOffspringCount(1);
        }

        // total up offspring
        for (final Species species : list) {
            total += species.getOffspringCount();
        }

        // how does the total offspring count match the target
        int diff = this.population.getPopulationSize() - total;

        if (diff < 0) {
            // need less offspring
            int index = list.size() - 1;
            while ((diff != 0) && (index > 0)) {
                final Species species = list.get(index);
                final int t = Math.min(species.getOffspringCount(),
                        Math.abs(diff));
                species.setOffspringCount(species.getOffspringCount() - t);
                if (species.getOffspringCount() == 0) {
                    list.remove(index);
                }
                diff += t;
                index--;
            }
        } else {
            // need more offspring
            list.get(0).setOffspringCount(
                    list.get(0).getOffspringCount() + diff);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void performSpeciation(List<Genome> genomeList) {
        final List<Genome> newGenomeList = resetSpecies(genomeList);
        speciateAndCalculateSpawnLevels(newGenomeList);
    }

    /**
     * Reset for an iteration.
     *
     * @return The genomes to speciate.
     */
    private List<Genome> resetSpecies(List<Genome> inputGenomes) {
        final List<Genome> result = new ArrayList<Genome>();
        final Object[] speciesArray = this.population.getSpecies().toArray();

        // Add the genomes
        for (final Genome genome : inputGenomes) {
            result.add(genome);
        }

        for (final Object element : speciesArray) {
            final BasicSpecies s = (BasicSpecies) element;
            s.purge();

            // did the leader die? If so, disband the species. (but don't kill
            // the genomes)
            if (!inputGenomes.contains(s.getLeader())) {
                removeSpecies(s);
            } else if (s.getGensNoImprovement() > this.numGensAllowedNoImprovement) {
                removeSpecies(s);
            }

            // remove the leader from the list we return. the leader already has
            // a species
            result.remove(s.getLeader());
        }

        if (this.population.getSpecies().size() == 0) {
            throw new AIFHError("Can't speciate, the population is empty.");
        }

        return result;
    }

    /**
     * @param compatibilityThreshold the compatibilityThreshold to set
     */
    public void setCompatibilityThreshold(final double compatibilityThreshold) {
        this.compatibilityThreshold = compatibilityThreshold;
    }

    /**
     * @param maxNumberOfSpecies the maxNumberOfSpecies to set
     */
    public void setMaxNumberOfSpecies(final int maxNumberOfSpecies) {
        this.maxNumberOfSpecies = maxNumberOfSpecies;
    }

    /**
     * @param numGensAllowedNoImprovement the numGensAllowedNoImprovement to set
     */
    public void setNumGensAllowedNoImprovement(
            final int numGensAllowedNoImprovement) {
        this.numGensAllowedNoImprovement = numGensAllowedNoImprovement;
    }

    /**
     * @param sortGenomes the sortGenomes to set
     */
    public void setSortGenomes(final SortGenomesForSpecies sortGenomes) {
        this.sortGenomes = sortGenomes;
    }

    /**
     * Determine the species.
     *
     * @param genomes The genomes to speciate.
     */
    private void speciateAndCalculateSpawnLevels(final List<Genome> genomes) {
        double maxScore = 0;

        if (genomes.size() == 0) {
            throw new AIFHError("Can't speciate, the population is empty.");
        }

        final List<Species> speciesCollection = this.population.getSpecies();

        if (speciesCollection.size() == 0) {
            throw new AIFHError("Can't speciate, there are no species.1");
        }

        // calculate compatibility between genomes and species
        adjustCompatibilityThreshold();

        // assign genomes to species (if any exist)
        for (final Genome genome : genomes) {
            Species currentSpecies = null;

            if (!Double.isNaN(genome.getAdjustedScore())
                    && !Double.isInfinite(genome.getAdjustedScore())) {
                maxScore = Math.max(genome.getAdjustedScore(), maxScore);
            }

            for (final Species s : speciesCollection) {
                final double compatibility = getCompatibilityScore(genome,
                        s.getLeader());

                if (compatibility <= this.compatibilityThreshold) {
                    currentSpecies = s;
                    addSpeciesMember(s, genome);
                    genome.setSpecies(s);
                    break;
                }
            }

            // if this genome did not fall into any existing species, create a
            // new species
            if (currentSpecies == null) {
                currentSpecies = new BasicSpecies(this.population, genome);
                this.population.getSpecies().add(currentSpecies);
            }
        }

        //
        double totalSpeciesScore = 0;
        for (final Species species : speciesCollection) {
            totalSpeciesScore += species.calculateShare(this.owner
                    .getScoreFunction().shouldMinimize(), maxScore);
        }

        if (speciesCollection.size() == 0) {
            throw new AIFHError("Can't speciate, there are no species.2");
        }
        if (totalSpeciesScore < AIFH.DEFAULT_PRECISION) {
            // This should not happen much, or if it does, only in the
            // beginning.
            // All species scored zero. So they are all equally bad. Just divide
            // up the right to produce offspring evenly.
            divideEven(speciesCollection);
        } else {
            // Divide up the number of offspring produced to the most fit
            // species.
            divideByFittestSpecies(speciesCollection, totalSpeciesScore);
        }

        levelOff();

    }

    /**
     * Determine how compatible two genomes are. More compatible genomes will be
     * placed into the same species. The lower the number, the more compatible.
     *
     * @param genome1 The first genome.
     * @param genome2 The second genome.
     * @return The compatibility level.
     */
    public abstract double getCompatibilityScore(Genome genome1, Genome genome2);
}
