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

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    /// An array genome represents an array of "something", this allows array
    /// operators such as crossover and mutate to work on the genome.
    /// </summary>
    public interface IArrayGenome : IGenome
    {
        /// <summary>
        /// Copy elements from another array genome into this one.
        /// </summary>
        /// <param name="source">The source genome.</param>
        /// <param name="sourceIndex">The source index.</param>
        /// <param name="targetIndex">The target index.</param>
        void Copy(IArrayGenome source, int sourceIndex, int targetIndex);
        
        /// <summary>
        /// Swap two elements in this genome. 
        /// </summary>
        /// <param name="iswap1">The first element index to swap.</param>
        /// <param name="iswap2">The second element index to swap.</param>
        void Swap(int iswap1, int iswap2);
    }
}
