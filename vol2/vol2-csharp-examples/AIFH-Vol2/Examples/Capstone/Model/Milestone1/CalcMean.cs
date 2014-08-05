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
namespace AIFH_Vol2.Examples.Capstone.Model.Milestone1
{
    /// <summary>
    /// Calculate the mean of a series of doubles.
    /// </summary>
    public class CalcMean
    {
        /// <summary>
        ///     How many values have we encountered so far.
        /// </summary>
        private int _count;

        /// <summary>
        ///     What is the sum of values.
        /// </summary>
        private double _sum;

        /// <summary>
        ///     Update mean for a new value.
        /// </summary>
        /// <param name="d">The next value.</param>
        public void Update(double d)
        {
            _sum += d;
            _count++;
        }

        /// <summary>
        ///     The calculated mean.
        /// </summary>
        /// <returns>The calculated mean.</returns>
        public double Calculate()
        {
            return _sum/_count;
        }
    }
}
