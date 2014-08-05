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
namespace AIFH_Vol2.Core.Randomize
{
    /// <summary>
    /// Provides a foundation for most random number generation.  This allows the nextDouble to generate
    /// the other types.
    /// </summary>
    public abstract class AbstractGenerateRandom : IGenerateRandom
    {
        /// <inheritdoc/>
        public int NextInt(int low, int high)
        {
            return (low + (int)(NextDouble() * ((high - low))));
        }

        /// <inheritdoc/>
        public double NextDouble(double high)
        {
            return NextDouble(0, high);
        }

        /// <inheritdoc/>
        public double NextDouble(double low, double high)
        {
            return (low + (NextDouble() * ((high - low))));
        }

        /// <inheritdoc/>
        public int NextInt(int range)
        {
            return NextInt(0, range);
        }

        /// <inheritdoc/>
        public abstract double NextDouble();

        /// <inheritdoc/>
        public abstract bool NextBoolean();

        /// <inheritdoc/>
        public abstract float NextFloat();

        /// <inheritdoc/>
        public abstract double NextGaussian();

        /// <inheritdoc/>
        public abstract long NextLong();

        /// <inheritdoc/>
        public abstract int NextInt();
    }
}
