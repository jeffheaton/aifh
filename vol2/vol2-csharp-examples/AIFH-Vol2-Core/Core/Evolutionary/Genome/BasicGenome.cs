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
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Genome
{
    /// <summary>
    /// A basic abstract genome. Provides base functionality.
    /// </summary>
    [Serializable]
    public abstract class BasicGenome : IGenome
    {
        /// <summary>
        /// The adjusted score. If unknown, it is set to NaN.
        /// </summary>
        public double AdjustedScore { get; set; }

        /// <summary>
        /// The score of this genome.
        /// </summary>
        public double Score { get; set; }

        /// <summary>
        /// The population this genome belongs to.
        /// </summary>
        public IPopulation Population { get; set; }

        /// <summary>
        /// The birth generation for this genome.
        /// </summary>
        public int BirthGeneration { get; set; }

        /// <summary>
        /// The species of this genome.
        /// </summary>
        public ISpecies Species { get; set; }

        /// <summary>
        /// Construct a basic genome.
        /// </summary>
        public BasicGenome()
        {
            Score = double.NaN;
            AdjustedScore = double.NaN;
        }

        /// <inheritdoc/>
        public String ToString()
        {
            StringBuilder builder = new StringBuilder();
            builder.Append("[");
            builder.Append(this.GetType().Name);
            builder.Append(": score=");
            builder.Append(Score);
            return builder.ToString();
        }


        /// <inheritdoc/>
        public abstract void Copy(IGenome source);

        /// <inheritdoc/>
        public abstract int Count { get; }

        /// <inheritdoc/>
        public abstract double[] LongTermMemory { get; }
    }
}
