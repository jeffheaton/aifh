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
    /// Interface that defines how random numbers are generated.  Provides the means to generate both uniform 
    /// and normal (gaussian) distributed random numbers.
    /// </summary>
    public interface IGenerateRandom
    {
        /// <summary>
        /// Generate the next normally distributed random number.
        /// </summary>
        /// <returns>The next normally distributed random number.</returns>
        double NextGaussian();

        /// <summary>
        /// Generate the next random boolean.
        /// </summary>
        /// <returns>The next random boolean.</returns>
        bool NextBoolean();

        /// <summary>
        /// Generate the next random long.
        /// </summary>
        /// <returns>The next random long.</returns>
        long NextLong();

        /// <summary>
        /// Generate the next random floating point.
        /// </summary>
        /// <returns>The next random floating point.</returns>
        float NextFloat();

        /// <summary>
        /// Generate the next random double.
        /// </summary>
        /// <returns>The next random double.</returns>
        double NextDouble();

        /// <summary>
        /// The next random double up to a non-inclusive range. 
        /// </summary>
        /// <param name="high">The highest desired value.</param>
        /// <returns>The result.</returns>
        double NextDouble(double high);

        /// <summary>
        /// The next double between low (inclusive) and high (exclusive). 
        /// </summary>
        /// <param name="low">The inclusive low value.</param>
        /// <param name="high">The exclusive high value.</param>
        /// <returns>The result.</returns>
        double NextDouble(double low, double high);

        /// <summary>
        /// Generate the next random integer.
        /// </summary>
        /// <returns>The next random integer.</returns>
        int NextInt();

        /// <summary>
        /// The next random int up to a non-inclusive range. 
        /// </summary>
        /// <param name="high">The highest desired value.</param>
        /// <returns>The result.</returns>
        int NextInt(int high);

        /// <summary>
        /// The next int between low (inclusive) and high (exclusive). 
        /// </summary>
        /// <param name="low">The inclusive low value.</param>
        /// <param name="high">The exclusive high value.</param>
        /// <returns>The result.</returns>
        int NextInt(int low, int high);
    }
}
