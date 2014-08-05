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
namespace AIFH_Vol2_Flocking
{
    /// <summary>
    ///     A particle.
    /// </summary>
    public class Particle
    {
        /// <summary>
        ///     The location.
        /// </summary>
        private readonly double[] _location;

        /// <summary>
        ///     The velocity.
        /// </summary>
        private readonly double[] _velocity;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="dimensions">The dimensions.</param>
        public Particle(int dimensions)
        {
            _location = new double[dimensions];
            _velocity = new double[dimensions];
        }

        /// <summary>
        ///     The location vector.
        /// </summary>
        public double[] Location
        {
            get { return _location; }
        }

        /// <summary>
        ///     The velocity vector.
        /// </summary>
        public double[] Velocity
        {
            get { return _velocity; }
        }
    }
}
