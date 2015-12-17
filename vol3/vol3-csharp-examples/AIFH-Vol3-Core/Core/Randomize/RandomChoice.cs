// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
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

namespace AIFH_Vol3.Core.Randomize
{
    /// <summary>
    ///     Generate random choices unevenly.  This class is used to select random
    ///     choices from a list, with a probability weight places on each item
    ///     in the list.
    ///     This is often called a Roulette Wheel in Machine Learning texts.  How it differs from
    ///     a Roulette Wheel that you might find in Las Vegas or Monte Carlo is that the
    ///     areas that can be selected are not of uniform size.  However, you can be sure
    ///     that one will be picked.
    ///     <p />
    ///     http://en.wikipedia.org/wiki/Fitness_proportionate_selection
    /// </summary>
    [Serializable]
    public class RandomChoice
    {
        /// <summary>
        ///     The probabilities of each item in the list.
        /// </summary>
        private readonly double[] _probabilities;

        /// <summary>
        ///     Construct a list of probabilities.
        /// </summary>
        /// <param name="theProbabilities">The probability of each item in the list.</param>
        public RandomChoice(double[] theProbabilities)
        {
            _probabilities = (double[]) theProbabilities.Clone();

            double total = 0;
            foreach (var probability in _probabilities)
            {
                total += probability;
            }

            if (total == 0.0)
            {
                var prob = 1.0/_probabilities.Length;
                for (var i = 0; i < _probabilities.Length; i++)
                {
                    _probabilities[i] = prob;
                }
            }
            else
            {
                double total2 = 0;
                var factor = 1.0/total;
                for (var i = 0; i < _probabilities.Length; i++)
                {
                    _probabilities[i] = _probabilities[i]*factor;
                    total2 += _probabilities[i];
                }

                if (Math.Abs(1.0 - total2) > 0.02)
                {
                    var prob = 1.0/_probabilities.Length;
                    for (var i = 0; i < _probabilities.Length; i++)
                    {
                        _probabilities[i] = prob;
                    }
                }
            }
        }

        /// <summary>
        ///     Generate a random choice, based on the probabilities provided to the constructor.
        /// </summary>
        /// <param name="theGenerator"></param>
        /// <returns>The random choice.</returns>
        public int Generate(IGenerateRandom theGenerator)
        {
            var r = theGenerator.NextDouble();
            var sum = 0.0;

            for (var i = 0; i < _probabilities.Length; i++)
            {
                sum += _probabilities[i];
                if (r < sum)
                {
                    return i;
                }
            }

            for (var i = 0; i < _probabilities.Length; i++)
            {
                if (_probabilities[i] != 0.0)
                {
                    return i;
                }
            }

            throw new AIFHError("Invalid probabilities.");
        }

        /// <summary>
        ///     Generate a random choice, but skip one of the choices.
        /// </summary>
        /// <param name="theGenerator">Random number generator.</param>
        /// <param name="skip">The choice to skip.</param>
        /// <returns>The random choice.</returns>
        public int Generate(IGenerateRandom theGenerator, int skip)
        {
            var totalProb = 1.0 - _probabilities[skip];

            var throwValue = theGenerator.NextDouble()*totalProb;
            var accumulator = 0.0;

            for (var i = 0; i < skip; i++)
            {
                accumulator += _probabilities[i];
                if (accumulator > throwValue)
                {
                    return i;
                }
            }

            for (var i = skip + 1; i < _probabilities.Length; i++)
            {
                accumulator += _probabilities[i];
                if (accumulator > throwValue)
                {
                    return i;
                }
            }

            for (var i = 0; i < skip; i++)
            {
                if (_probabilities[i] != 0.0)
                {
                    return i;
                }
            }
            for (var i = skip + 1; i < _probabilities.Length; i++)
            {
                if (_probabilities[i] != 0.0)
                {
                    return i;
                }
            }

            return -1;
        }
    }
}