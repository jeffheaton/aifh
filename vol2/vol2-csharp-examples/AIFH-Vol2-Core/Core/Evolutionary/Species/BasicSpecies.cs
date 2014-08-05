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
using AIFH_Vol2.Core.Evolutionary.Population;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Species
{
    /// <summary>
    /// Provides basic functionality for a species.
    /// </summary>
    [Serializable]
    public class BasicSpecies: ISpecies
    {
        /// <summary>
        ///     The list of genomes.
        /// </summary>
        private List<IGenome> _members = new List<IGenome>();

        /// <summary>
        ///     Default constructor, used mainly for persistence.
        /// </summary>
        public BasicSpecies()
        {
        }

        /// <summary>
        ///     Construct a species.
        /// </summary>
        /// <param name="thePopulation">The population the species belongs to.</param>
        /// <param name="theFirst">The first genome in the species.</param>
        public BasicSpecies(IPopulation thePopulation, IGenome theFirst)
        {
            Population = thePopulation;
            BestScore = theFirst.Score;
            GensNoImprovement = 0;
            Age = 0;
            Leader = theFirst;
            _members.Add(theFirst);
        }

        /// <summary>
        ///     The age of this species.
        /// </summary>
        public int Age { get; set; }

        /// <summary>
        ///     The best score.
        /// </summary>
        public double BestScore { get; set; }

        /// <summary>
        ///     The number of generations with no improvement.
        /// </summary>
        public int GensNoImprovement { get; set; }

        /// <summary>
        ///     The leader.
        /// </summary>
        public IGenome Leader { get; set; }

        /// <summary>
        ///     The owner class.
        /// </summary>
        public IPopulation Population { get; set; }

        /// <summary>
        ///     The offspring count.
        /// </summary>
        public int OffspringCount { get; set; }

        /// <summary>
        ///     The offpsring share (percent).
        /// </summary>
        public double OffspringShare { get; set; }

        /// <inheritdoc />
        public void Add(IGenome genome)
        {
            genome.Population = Population;
            _members.Add(genome);
        }

        /// <inheritdoc />
        public double CalculateShare(bool shouldMinimize,
                                     double maxScore)
        {
            double total = 0;

            int count = 0;
            foreach (IGenome genome in _members)
            {
                if (!double.IsNaN(genome.AdjustedScore)
                    && !double.IsInfinity(genome.AdjustedScore))
                {
                    double s;
                    if (shouldMinimize)
                    {
                        s = maxScore - genome.AdjustedScore;
                    }
                    else
                    {
                        s = genome.AdjustedScore;
                    }
                    total += s;
                    count++;
                }
            }

            if (count == 0)
            {
                OffspringShare = 0;
            }
            else
            {
                OffspringShare = total / count;
            }

            return OffspringShare;
        }

        /// <inheritdoc />
        public List<IGenome> Members
        {
            get { return _members; }
            set { _members = value; }
        }

        /// <summary>
        ///     Purge all members, increase age by one and count the number of
        ///     generations with no improvement.
        /// </summary>
        public void Purge()
        {
            _members.Clear();
            if (Leader != null)
            {
                _members.Add(Leader);
            }
            Age++;
            GensNoImprovement++;
            OffspringCount = 0;
            OffspringShare = 0;
        }


        /// <inheritdoc />
        public override String ToString()
        {
            var result = new StringBuilder();
            result.Append("[BasicSpecies: score=");
            result.Append(BestScore);
            result.Append(", members=");
            result.Append(_members.Count);
            result.Append(", age=");
            result.Append(Age);
            result.Append(", no_improv=");
            result.Append(GensNoImprovement);
            result.Append(", share=");
            result.Append(OffspringShare);
            result.Append(", offspring count=");
            result.Append(OffspringShare);
            result.Append("]");
            return result.ToString();
        }

    }
}
