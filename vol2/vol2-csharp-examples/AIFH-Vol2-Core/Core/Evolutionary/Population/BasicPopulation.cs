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
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Species;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Population
{
    /// <summary>
    /// Defines the basic functionality for a population of genomes. The population
    /// is made up of species. These species contain the individiual genomes that
    /// make up the population. If you do not want to use species, then create one/
    /// species that holds every genome.
    /// </summary>
    [Serializable]
    public class BasicPopulation : IPopulation
    {
        /// <summary>
        /// The species that make up the population.
        /// </summary>
        private List<ISpecies> _species = new List<ISpecies>();

        /// <summary>
        /// The best genome.
        /// </summary>
        public IGenome BestGenome { get; set; }

        /// <summary>
        /// A factory that can be used to store create genomes.
        /// </summary>
        public IGenomeFactory GenomeFactory { get; set; }

        /// <summary>
        /// How many genomes should be created.
        /// </summary>
        public int PopulationSize { get; set; }

        /// <summary>
        /// Construct an empty population.
        /// </summary>
        public BasicPopulation()
        {
            PopulationSize = 0;
        }

        /// <summary>
        /// Construct a population. 
        /// </summary>
        /// <param name="thePopulationSize"></param>
        /// <param name="theGenomeFactory"></param>
        public BasicPopulation(int thePopulationSize,
                               IGenomeFactory theGenomeFactory)
        {
            PopulationSize = thePopulationSize;
            GenomeFactory = theGenomeFactory;
        }

        /// <inheritdoc/>
        public void Clear()
        {
            _species.Clear();

        }

        /// <inheritdoc/>
        public ISpecies CreateSpecies()
        {
            ISpecies species = new BasicSpecies();
            species.Population = this;
            Species.Add(species);
            return species;
        }

        /// <inheritdoc/>
        public ISpecies DetermineBestSpecies()
        {
            foreach (ISpecies species in _species)
            {
                if (species.Members.Contains(BestGenome))
                {
                    return species;
                }
            }
            return null;
        }

        /// <inheritdoc/>
        public IList<IGenome> Flatten()
        {
            IList<IGenome> result = new List<IGenome>();
            foreach (ISpecies species in _species)
            {
                foreach (IGenome genome in species.Members)
                {
                    result.Add(genome);
                }
            }
            return result;
        }

        /// <inheritdoc/>
        public int MaxIndividualSize
        {
            get
            {
                return int.MaxValue;
            }
        }

        /// <inheritdoc/>
        public List<ISpecies> Species
        {
            get
            {
                return _species;
            }
        }

        /// <inheritdoc/>
        public int Count
        {
            get
            {
                return Flatten().Count;
            }
        }

        /// <summary>
        /// Not supported, a population cannot be fit by methods that use getLongTermMemory.
        /// The population is made up of many elements. 
        /// </summary>
        public double[] LongTermMemory
        {
            get
            {
                throw new AIFHError("Can't get long term memory for entire population.");
            }
        }

        /// <summary>
        /// Purge any invalid genomes.
        /// </summary>
        public void PurgeInvalidGenomes()
        {
            // remove any invalid genomes
            int speciesNum = 0;
            while (speciesNum < Species.Count)
            {
                ISpecies species = Species[speciesNum];

                int genomeNum = 0;
                while (genomeNum < species.Members.Count)
                {
                    IGenome genome = species.Members[genomeNum];
                    if (double.IsInfinity(genome.Score)
                            || double.IsInfinity(genome.AdjustedScore)
                            || double.IsNaN(genome.Score)
                            || double.IsNaN(genome.AdjustedScore))
                    {
                        species.Members.Remove(genome);
                    }
                    else
                    {
                        genomeNum++;
                    }
                }

                // is the species now empty?
                if (species.Members.Count == 0)
                {
                    Species.Remove(species);
                }
                else
                {
                    // new leader needed?
                    if (!species.Members.Contains(species.Leader))
                    {
                        species.Leader = species.Members[0];
                        species.BestScore = species.Leader.Score;
                    }

                    // onto the next one!
                    speciesNum++;
                }
            }
        }
    }

}
