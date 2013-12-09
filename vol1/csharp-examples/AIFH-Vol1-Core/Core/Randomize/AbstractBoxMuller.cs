// Artificial Intelligence for Humans
// Volume 1: Fundamental Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2013 by Jeff Heaton
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

namespace AIFH_Vol1.Core.Randomize
{
    /// <summary>
    /// Provides the ability for subclasses to generate normally distributed random numbers.
    /// </summary>
    public abstract class AbstractBoxMuller : AbstractGenerateRandom
    {
        /// <summary>
        /// The y2 value.
        /// </summary>
        private double _y2;

        /// <summary>
        /// Should we use the last value.
        /// </summary>
        private bool _useLast;

        /// <summary>
        /// The mean.
        /// </summary>
        public const double Mu = 0;

        /// <summary>
        /// The standard deviation.
        /// </summary>
        private const double Sigma = 1;


        /// <inheritdoc/>
        public override double NextGaussian()
        {
            double y1;

            // use value from previous call
            if (_useLast)
            {
                y1 = _y2;
                _useLast = false;
            }
            else
            {
                double x1;
                double x2;
                double w;
                do
                {
                    x1 = 2.0 * NextDouble() - 1.0;
                    x2 = 2.0 * NextDouble() - 1.0;
                    w = x1 * x1 + x2 * x2;
                } while (w >= 1.0);

                w = Math.Sqrt((-2.0 * Math.Log(w)) / w);
                y1 = x1 * w;
                _y2 = x2 * w;
                _useLast = true;
            }

            return (Mu + y1 * Sigma);
        }

        /// <inheritdoc/>
        public abstract override double NextDouble();

        /// <inheritdoc/>
        public abstract override bool NextBoolean();

        /// <inheritdoc/>
        public abstract override float NextFloat();

        /// <inheritdoc/>
        public abstract override long NextLong();

        /// <inheritdoc/>
        public abstract override int NextInt();

    }
}
