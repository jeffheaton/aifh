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
using AIFH_Vol2_Capstone_Plants.Milestone1;

namespace AIFH_Vol2_Capstone_Plants.Milestone2
{
    /// <summary>
    ///     The physics class defines limits on the growth that the genome wants to implement.  Specifically, the physics
    ///     determines how sunlight is absorbed and nourishment is distributed in the plant.
    ///     Sunlight comes from above and stops at the ground level. More leafy material absorbs sunlight and reduces it
    ///     because of shade.  Water comes from below and is absorbed by the roots.
    /// </summary>
    public class PlantPhysics
    {
        /**
     * Distribute the sunlight energy in the universe.
     *
     * @param universe The universe.
     */

        private void DistributeEnergy(PlantUniverse universe)
        {
            // Distribute sun energy downward
            var sunlight = new double[PlantUniverse.UniverseWidth];
            for (int i = 0; i < sunlight.Length; i++)
            {
                sunlight[i] = 1.0;
            }

            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    // no sun underground
                    double decay = row >= PlantUniverse.GroundLine ? 0 : 1;

                    PlantUniverseCell cell = universe.GetCell(row, col);
                    cell.CalculatedSunlight = sunlight[col];

                    // Collect resources for live cells
                    if (cell.IsAlive)
                    {
                        // Live cells cause the sunlight to decay (shade)
                        decay *= PlantUniverse.Decay*cell.Leafyness;

                        // Set the energy based on sunlight level and composition of the live cell
                        double myEnergy = cell.CalculatedSunlight*cell.Leafyness;
                        double transEnergy = universe.CalculateTransferEnergy(row, col)*(1.0 - cell.Leafyness);
                        double e = Math.Max(myEnergy, transEnergy);
                        e = Math.Max(PlantUniverse.MinLivingEnergy, e);
                        cell.Energy = e;
                    }

                    sunlight[col] *= decay;
                }
            }
        }

        /**
         * Distribute nourishment in the universe.
         *
         * @param universe The universe.
         */

        private void DistributeNourishment(PlantUniverse universe)
        {
            double rootCount = 0;
            double surfaceCount = 0;

            // Distribute sun energy downward
            var waterTable = new double[PlantUniverse.UniverseWidth];
            for (int i = 0; i < waterTable.Length; i++)
            {
                waterTable[i] = 1.0;
            }

            for (int row = PlantUniverse.UniverseHeight - 1; row >= 0; row--)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    double decay;

                    // no water above ground
                    decay = row < PlantUniverse.GroundLine ? 0 : 1;

                    PlantUniverseCell cell = universe.GetCell(row, col);
                    cell.CalculatedWater = waterTable[col];

                    // Collect resources for live cells
                    if (cell.IsAlive)
                    {
                        // Live cells cause the water to decay (roots collect)
                        decay *= PlantUniverse.Decay;

                        // Set the energy based on sunlight level and composition of the live cell
                        double myWater = cell.CalculatedWater*cell.Leafyness;
                        double transWater = universe.CalculateTransferNourishment(row, col)*(1.0 - cell.Leafyness);
                        double n = Math.Max(myWater, transWater);
                        n = Math.Max(PlantUniverse.MinLivingEnergy, n);
                        cell.Nourishment = n;

                        // update the root and surface counts
                        if (row >= PlantUniverse.GroundLine)
                        {
                            rootCount += cell.Nourishment;
                        }
                        else
                        {
                            surfaceCount += cell.Leafyness;
                        }
                    }

                    waterTable[col] *= decay;
                }
            }

            universe.RootCount = rootCount;
            universe.SurfaceCount = surfaceCount;
        }


        public void RunPhysics(PlantUniverse universe)
        {
            DistributeEnergy(universe);
            DistributeNourishment(universe);
        }
    }
}
