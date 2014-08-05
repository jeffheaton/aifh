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
using AIFH_Vol2.Core;

namespace AIFH_Vol2_Capstone_Plants.Milestone1
{
    /// <summary>
    ///     An individual cell in the plant universe.
    /// </summary>
    public class PlantUniverseCell
    {
        /// <summary>
        ///     How green (leaf) or brown (trunk) is the cell.  1.0 is fully leaf, 0.0 is fully trunk.
        /// </summary>
        public double Leafyness { get; set; }

        /// <summary>
        ///     The amount of energy between [0,1].
        /// </summary>
        public double Energy { get; set; }

        /// <summary>
        ///     The amount of nourishment between [0,1].
        /// </summary>
        public double Nourishment { get; set; }

        /// <summary>
        ///     The calculated sunlight exposure.
        /// </summary>
        public double CalculatedSunlight { get; set; }

        /// <summary>
        ///     The calculated water exposure.
        /// </summary>
        public double CalculatedWater { get; set; }

        /// <summary>
        ///     True, if this cell is alive.
        /// </summary>
        public bool IsAlive
        {
            get { return Energy > AIFH.DefaultPrecision; }
        }
    }
}
