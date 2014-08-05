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
using AIFH_Vol2.Core.Learning;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Genome
{
    /// <summary>
    /// A genome is the basic blueprint for creating an phenome (organism) in Encog.
    /// Some genomes also function as phenomes.
    /// </summary>
    public interface IGenome: IMLMethod
    {
        /// <summary>
        /// Copy from the specified genome into this one.
        /// </summary>
        /// <param name="source">The source genome.</param>
        void Copy(IGenome source);

        /// <summary>
        /// Get the adjusted score, this considers old-age penalties and youth
        /// bonuses. If there are no such bonuses or penalties, this is the same as
        /// the score. 
        /// </summary>
        double AdjustedScore { get; set; }

        /// <summary>
        /// The birth generation (or iteration).
        /// </summary>
        int BirthGeneration { get; set; }

        /// <summary>
        /// The population that this genome belongs to.
        /// </summary>
        IPopulation Population { get; set; }

        /// <summary>
        /// The score for this genome.
        /// </summary>
        double Score { get; set; }

        /// <summary>
        /// Return the size of this genome. This size is a relative number
        /// that indicates the complexity of the genome.
        /// </summary>
        int Count { get; }


        /// <summary>
        /// The species for this genome.
        /// </summary>
        ISpecies Species { get; set; }
    }
}
