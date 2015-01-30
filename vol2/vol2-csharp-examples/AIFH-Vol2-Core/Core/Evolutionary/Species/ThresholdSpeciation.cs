// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//
using System;
using System.Linq;
using AIFH_Vol2.Core.Evolutionary.Sort;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Genome;
using System.Collections.Generic;

namespace AIFH_Vol2.Core.Evolutionary.Species
{
    /// <summary>
    ///     Speciate based on threshold. Any genomes with a compatibility score below a
    ///     level will be in the same species.
    /// </summary>
    [Serializable]
    public abstract class ThresholdSpeciation : ISpeciation
    {
        /// <summary>
        ///     The minimum compatibility that two genes must have to be in the same
        ///     species.
        /// </summary>
        private double _compatibilityThreshold = 1.0;

        /// <summary>
        ///     The maximum number of species. This is just a target. If the number of
        ///     species goes over this number then the compatibilityThreshold is
        ///     increased to decrease the number of species.
        /// </summary>
        private int _maxNumberOfSpecies = 40;

        /// <summary>
        ///     The maximum number of generations allows with no improvement. After this
        ///     the genomes in this species are not allowed to reproduce or continue.
        ///     This does not apply to top species.
        /// </summary>
        private int _numGensAllowedNoImprovement = 15;

        /// <summary>
        ///     The training being used.
        /// </summary>
        private IEvolutionaryAlgorithm _owner;

        /// <summary>
        ///     The population.
        /// </summary>
        private IPopulation _population;

        /// <summary>
        ///     The method used to sort the genomes in the species. More desirable
        ///     genomes should come first for later selection.
        /// </summary>
        private SortGenomesForSpecies _sortGenomes;

        /// <summary>
        ///     The minimum compatibility that two genes must have to be in the same
        ///     species.
        /// </summary>
        public double CompatibilityThreshold
        {
            get { return _compatibilityThreshold; }
            set { _compatibilityThreshold = value; }
        }

        /// <summary>
        ///     The maximum number of species. This is just a target. If the number of
        ///     species goes over this number then the compatibilityThreshold is
        ///     increased to decrease the number of species.
        /// </summary>
        public int MaxNumberOfSpecies
        {
            get { return _maxNumberOfSpecies; }
            set { _maxNumberOfSpecies = value; }
        }

        /// <summary>
        ///     The maximum number of generations allows with no improvement. After this
        ///     the genomes in this species are not allowed to reproduce or continue.
        ///     This does not apply to top species.
        /// </summary>
        public int NumGensAllowedNoImprovement
        {
            get { return _numGensAllowedNoImprovement; }
            set { _numGensAllowedNoImprovement = value; }
        }

        /// <summary>
        ///     The owner.
        /// </summary>
        public IEvolutionaryAlgorithm Owner
        {
            get { return _owner; }
        }

        /// <summary>
        ///     The method used to sort the genomes in the species. More desirable
        ///     genomes should come first for later selection.
        /// </summary>
        public SortGenomesForSpecies SortGenomes
        {
            get { return _sortGenomes; }
            set { _sortGenomes = value; }
        }

        /// <inheritdoc />
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            _owner = theOwner;
            _population = theOwner.Population;
            _sortGenomes = new SortGenomesForSpecies(_owner);
        }

        /// <inheritdoc />
        public void PerformSpeciation(IList<IGenome> genomeList)
        {
            IList<IGenome> newGenomeList = ResetSpecies(genomeList);
            SpeciateAndCalculateSpawnLevels(newGenomeList);
        }

        /// <summary>
        ///     Add a genome.
        /// </summary>
        /// <param name="species">The species to add to.</param>
        /// <param name="genome">The genome to add.</param>
        public void AddSpeciesMember(ISpecies species, IGenome genome)
        {
            if (_owner.ValidationMode)
            {
                if (species.Members.Contains(genome))
                {
                    throw new AIFHError("Species already contains genome: "
                                           + genome);
                }
            }

            if (_owner.SelectionComparer.Compare(genome,
                                                species.Leader) < 0)
            {
                species.BestScore = genome.AdjustedScore;
                species.GensNoImprovement = 0;
                species.Leader = genome;
            }

            species.Add(genome);
        }

        /// <summary>
        ///     Adjust the species compatibility threshold. This prevents us from having
        ///     too many species. Dynamically increase or decrease the
        ///     compatibilityThreshold.
        /// </summary>
        private void AdjustCompatibilityThreshold()
        {
            // has this been disabled (unlimited species)
            if (_maxNumberOfSpecies < 1)
            {
                return;
            }

            const double thresholdIncrement = 0.01;

            if (_population.Species.Count > _maxNumberOfSpecies)
            {
                _compatibilityThreshold += thresholdIncrement;
            }

            else if (_population.Species.Count < 2)
            {
                _compatibilityThreshold -= thresholdIncrement;
            }
        }

        /// <summary>
        ///     Divide up the potential offspring by the most fit species. To do this we
        ///     look at the total species score, vs each individual species percent
        ///     contribution to that score.
        /// </summary>
        /// <param name="speciesCollection">The current species list.</param>
        /// <param name="totalSpeciesScore">The total score over all species.</param>
        private void DivideByFittestSpecies(IEnumerable<ISpecies> speciesCollection,
                                            double totalSpeciesScore)
        {
            ISpecies bestSpecies = FindBestSpecies();

            // loop over all species and calculate its share
            ISpecies[] speciesArray = speciesCollection.ToArray();
            foreach (ISpecies element in speciesArray)
            {
                var species = element;
                // calculate the species share based on the percent of the total
                // species score
                var share = (int)Math
                                      .Round((species.OffspringShare / totalSpeciesScore)
                                             * _owner.Population.PopulationSize);

                // do not give the best species a zero-share
                if ((species == bestSpecies) && (share == 0))
                {
                    share = 1;
                }

                // if the share is zero, then remove the species
                if ((species.Members.Count == 0) || (share == 0))
                {
                    RemoveSpecies(species);
                }
                // if the species has not improved over the specified number of
                // generations, then remove it.
                else if ((species.GensNoImprovement > _numGensAllowedNoImprovement)
                         && (species != bestSpecies))
                {
                    RemoveSpecies(species);
                }
                else
                {
                    // otherwise assign a share and sort the members.
                    species.OffspringCount = share;
                    species.Members.Sort(_sortGenomes);
                }
            }
        }

        /// <summary>
        ///     Find the best species.
        /// </summary>
        /// <returns>The best species.</returns>
        public ISpecies FindBestSpecies()
        {
            if (_owner.BestGenome != null)
            {
                return _owner.BestGenome.Species;
            }
            return null;
        }

        /// <summary>
        ///     Attempt to remove a removable species. If the species is the best
        ///     species, then do not remove it. If the species is the last species, don't
        ///     remove it.
        /// </summary>
        /// <param name="species">The species to attempt to remove.</param>
        public void RemoveSpecies(ISpecies species)
        {
            if (species != FindBestSpecies())
            {
                if (_population.Species.Count > 1)
                {
                    _population.Species.Remove(species);
                }
            }
        }

        /// <summary>
        ///     If no species has a good score then divide the potential offspring amount
        ///     all species evenly.
        /// </summary>
        /// <param name="speciesCollection">The current set of species.</param>
        private void DivideEven(IList<ISpecies> speciesCollection)
        {
            double ratio = 1.0 / speciesCollection.Count;
            foreach (ISpecies species in speciesCollection)
            {
                var share = (int)Math.Round(ratio
                                             * _owner.Population.PopulationSize);
                species.OffspringCount = share;
            }
        }

        /// <summary>
        ///     Level off all of the species shares so that they add up to the desired
        ///     population size. If they do not add up to the desired species size, this
        ///     was a result of rounding the floating point share amounts to integers.
        /// </summary>
        private void LevelOff()
        {
            List<ISpecies> list = _population.Species;

            if (list.Count == 0)
            {
                throw new AIFHError(
                    "Can't speciate, next generation contains no species.");
            }

            list.Sort(new SpeciesComparer(_owner));

            // best species gets at least one offspring
            if (list[0].OffspringCount == 0)
            {
                list[0].OffspringCount = 1;
            }

            // total up offspring
            int total = list.Sum(species => species.OffspringCount);

            // how does the total offspring count match the target
            int diff = _population.PopulationSize - total;

            if (diff < 0)
            {
                // need less offspring
                int index = list.Count - 1;
                while ((diff != 0) && (index > 0))
                {
                    ISpecies species = list[index];
                    int t = Math.Min(species.OffspringCount,
                                     Math.Abs(diff));
                    species.OffspringCount = (species.OffspringCount - t);
                    if (species.OffspringCount == 0)
                    {
                        list.Remove(species);
                    }
                    diff += t;
                    index--;
                }
            }
            else
            {
                // need more offspring
                list[0].OffspringCount = (
                                             list[0].OffspringCount + diff);
            }
        }

        /// <summary>
        ///     Reset for an iteration.
        /// </summary>
        /// <param name="inputGenomes">The genomes to speciate.</param>
        /// <returns></returns>
        private IList<IGenome> ResetSpecies(IList<IGenome> inputGenomes)
        {
            ISpecies[] speciesArray = _population.Species.ToArray();

            // Add the genomes
            IList<IGenome> result = inputGenomes.ToList();

            foreach (ISpecies element in speciesArray)
            {
                var s = (BasicSpecies)element;
                s.Purge();

                // did the leader die? If so, disband the species. (but don't kill
                // the genomes)
                if (!inputGenomes.Contains(s.Leader))
                {
                    RemoveSpecies(s);
                }
                else if (s.GensNoImprovement > _numGensAllowedNoImprovement)
                {
                    RemoveSpecies(s);
                }

                // remove the leader from the list we return. the leader already has
                // a species
                result.Remove(s.Leader);
            }

            if (_population.Species.Count == 0)
            {
                throw new AIFHError("Can't speciate, the population is empty.");
            }

            return result;
        }

        /// <summary>
        ///     Determine the species.
        /// </summary>
        /// <param name="genomes">The genomes to speciate.</param>
        private void SpeciateAndCalculateSpawnLevels(IList<IGenome> genomes)
        {
            double maxScore = 0;

            if (genomes.Count == 0)
            {
                throw new AIFHError("Can't speciate, the population is empty.");
            }

            IList<ISpecies> speciesCollection = _population.Species;

            if (speciesCollection.Count == 0)
            {
                throw new AIFHError("Can't speciate, there are no species.1");
            }

            // calculate compatibility between genomes and species
            AdjustCompatibilityThreshold();

            // assign genomes to species (if any exist)
            foreach (IGenome g in genomes)
            {
                ISpecies currentSpecies = null;
                IGenome genome = g;

                if (!Double.IsNaN(genome.AdjustedScore)
                    && !double.IsInfinity(genome.AdjustedScore))
                {
                    maxScore = Math.Max(genome.AdjustedScore, maxScore);
                }

                foreach (ISpecies s in speciesCollection)
                {
                    double compatibility = GetCompatibilityScore(genome,
                                                                 s.Leader);

                    if (compatibility <= _compatibilityThreshold)
                    {
                        currentSpecies = s;
                        AddSpeciesMember(s, genome);
                        genome.Species = s;
                        break;
                    }
                }

                // if this genome did not fall into any existing species, create a
                // new species
                if (currentSpecies == null)
                {
                    currentSpecies = new BasicSpecies(_population, genome);
                    _population.Species.Add(currentSpecies);
                }
            }

            //
            double totalSpeciesScore = speciesCollection.Sum(species => species.CalculateShare(_owner.ScoreFunction.ShouldMinimize, maxScore));

            if (speciesCollection.Count == 0)
            {
                throw new AIFHError("Can't speciate, there are no species.2");
            }
            if (totalSpeciesScore < AIFH.DefaultPrecision)
            {
                // This should not happen much, or if it does, only in the
                // beginning.
                // All species scored zero. So they are all equally bad. Just divide
                // up the right to produce offspring evenly.
                DivideEven(speciesCollection);
            }
            else
            {
                // Divide up the number of offspring produced to the most fit
                // species.
                DivideByFittestSpecies(speciesCollection, totalSpeciesScore);
            }

            LevelOff();
        }

        /// <summary>
        ///     Determine how compatible two genomes are. More compatible genomes will be
        ///     placed into the same species. The lower the number, the more compatible.
        /// </summary>
        /// <param name="genome1">The first genome.</param>
        /// <param name="genome2">The second genome.</param>
        /// <returns>The compatibility level.</returns>
        public abstract double GetCompatibilityScore(IGenome genome1, IGenome genome2);
    }
}
