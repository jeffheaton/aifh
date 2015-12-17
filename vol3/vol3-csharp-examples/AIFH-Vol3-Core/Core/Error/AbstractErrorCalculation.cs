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

namespace AIFH_Vol3.Core.Error
{
    /// <summary>
    ///     An abstract error calculation class that provides some basic functionality.
    /// </summary>
    public abstract class AbstractErrorCalculation : IErrorCalculation
    {
        /// <summary>
        ///     The overall error.
        /// </summary>
        protected double GlobalError;

        /// <summary>
        ///     The size of a set.
        /// </summary>
        protected int GlobalSetSize;

        /// <inheritdoc />
        public void UpdateError(double[] actual, double[] ideal, double significance)
        {
            for (var i = 0; i < actual.Length; i++)
            {
                var delta = (ideal[i] - actual[i])*significance;

                GlobalError += delta*delta;
            }

            GlobalSetSize += ideal.Length;
        }

        /// <inheritdoc />
        public void UpdateError(double actual, double ideal)
        {
            var delta = ideal - actual;
            GlobalError += delta*delta;
            GlobalSetSize++;
        }

        /// <inheritdoc />
        public abstract double Calculate();

        /// <inheritdoc />
        public void Clear()
        {
            GlobalError = GlobalSetSize = 0;
        }

        /// <inheritdoc />
        public int SetSize
        {
            get { return GlobalSetSize; }
        }
    }
}