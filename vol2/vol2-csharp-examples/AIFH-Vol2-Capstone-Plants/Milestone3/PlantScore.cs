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
using AIFH_Vol2_Capstone_Plants.Milestone1;
using AIFH_Vol2_Capstone_Plants.Milestone2;

namespace AIFH_Vol2_Capstone_Plants.Milestone3
{
    /// <summary>
    ///     This class is used to score the plant.  Plants are scored for how green they are after a specified
    ///     number of iterations.
    /// </summary>
    public class PlantScore : IScoreFunction
    {
        /// <inheritdoc />
        public double CalculateScore(IMLMethod algo)
        {
            var genome = (DoubleArrayGenome) algo;
            var universe = new PlantUniverse();
            universe.Reset();
            var physics = new PlantPhysics();
            var growth = new PlantGrowth();

            // Run the generations.
            for (int i = 0; i < PlantUniverse.EvaluationCycles; i++)
            {
                physics.RunPhysics(universe);
                growth.RunGrowth(universe, genome.Data);
            }

            // Count the amount of green.
            int count = 0;
            double sum = 0;
            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    PlantUniverseCell cell = universe.GetCell(row, col);
                    if (cell.IsAlive)
                    {
                        if (row >= PlantUniverse.GroundLine)
                        {
                            sum += 0.5;
                        }
                        else
                        {
                            sum += cell.Leafyness;
                        }
                    }
                    count++;
                }
            }
            return sum/count;
        }

        /// <inheritdoc />
        public bool ShouldMinimize
        {
            get { return false; //To change body of implemented methods use File | Settings | File Templates.
            }
        }
    }
}
