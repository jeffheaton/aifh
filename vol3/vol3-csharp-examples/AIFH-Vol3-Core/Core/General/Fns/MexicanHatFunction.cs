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

namespace AIFH_Vol3.Core.General.Fns
{
    /// <summary>
    ///     The Mexican Hat, or Ricker wavelet, Radial Basis Function.
    ///     It is usually only referred to as the "Mexican hat" in the Americas, due to
    ///     cultural association with the "sombrero". In technical nomenclature this
    ///     function is known as the Ricker wavelet, where it is frequently employed to
    ///     model seismic data.
    ///     http://en.wikipedia.org/wiki/Mexican_Hat_Function
    /// </summary>
    public class MexicanHatFunction : AbstractRBF
    {
        /// <summary>
        ///     Construct the Mexican Hat RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
        /// </summary>
        /// <param name="theDimensions">The number of dimensions.</param>
        /// <param name="theParams">A vector to hold the parameters.</param>
        /// <param name="theIndex">The index into the params vector.  You can store multiple RBF's in a vector.</param>
        public MexicanHatFunction(int theDimensions, double[] theParams, int theIndex)
            : base(theDimensions, theParams, theIndex)
        {
        }

        /// <inheritdoc />
        public override double Evaluate(double[] x)
        {
            // calculate the "norm", but don't take square root
            // don't square because we are just going to square it
            double norm = 0;
            for (var i = 0; i < Dimensions; i++)
            {
                var center = GetCenter(i);
                norm += Math.Pow(x[i] - center, 2);
            }

            // calculate the value
            norm /= this.Width * this.Width;

            return (1 - norm)*Math.Exp(-norm/2);
        }
    }
}