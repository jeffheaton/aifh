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
using AIFH_Vol2_MergePhysics.Universe;

namespace AIFH_Vol2_MergePhysics.Physics
{
    /// <summary>
    ///     Defines a set of "physics" rules to control a cellular automation.
    /// </summary>
    public interface IPhysics
    {
        /// <summary>
        ///     Get the physical constants vector.
        /// </summary>
        double[] Data { get; }

        /// <summary>
        ///     Copy the physics constants vector from another array.
        /// </summary>
        /// <param name="sourceData">The source vector to copy.</param>
        void CopyData(double[] sourceData);

        /// <summary>
        ///     Load the physical constants vector from a text file.
        /// </summary>
        /// <param name="filename">The filename.</param>
        void Load(String filename);

        /// <summary>
        ///     Save the physical constants vector to a text file.
        /// </summary>
        /// <param name="filename">The filename.</param>
        void Save(String filename);


        /// <summary>
        ///     Perform the actual physics.
        /// </summary>
        /// <param name="outputUniverse">The new output universe.</param>
        /// <param name="row">The row of the cell we are processing.</param>
        /// <param name="col">The column of the cell we are processing.</param>
        void ProcessPixel(UniverseHolder outputUniverse, int row, int col);

        /// <summary>
        ///     Randomize the physics to random values.
        /// </summary>
        void Randomize();
    }
}
