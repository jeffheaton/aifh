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
namespace AIFH_Vol2.Core.General.Fns.Link
{
    /// <summary>
    /// The log link function for a GLM.
    ///
    /// http://en.wikipedia.org/wiki/Generalized_linear_model
    /// </summary>
    public class LogLinkFunction : IFn
    {
        /// <inheritdoc/>
        public double Evaluate(double[] x)
        {
            if (x.Length > 1)
            {
                throw new AIFHError("The logistic link function can only accept one parameter.");
            }
            return Math.Log(x[0]);
        }
    }
}
