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
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Examples.GA.TSP;

namespace AIFH_Vol2.Examples.GA.TSP
{
    /// <summary>
    ///     Calculate a score for the TSP.
    /// </summary>
    public class TSPScore : IScoreFunction
    {
        /// <summary>
        ///     The path of cities to visit.
        /// </summary>
        private readonly City[] _cities;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="cities">The cities.</param>
        public TSPScore(City[] cities)
        {
            _cities = cities;
        }

        /// <inheritdoc />
        public double CalculateScore(IMLMethod phenotype)
        {
            double result = 0.0;
            var genome = (IntegerArrayGenome) phenotype;
            int[] path = genome.Data;

            for (int i = 0; i < _cities.Length - 1; i++)
            {
                City city1 = _cities[path[i]];
                City city2 = _cities[path[i + 1]];

                double dist = city1.Proximity(city2);
                result += dist;
            }

            return result;
        }

        /// <inheritdoc />
        public bool ShouldMinimize
        {
            get { return true; }
        }
    }
}
