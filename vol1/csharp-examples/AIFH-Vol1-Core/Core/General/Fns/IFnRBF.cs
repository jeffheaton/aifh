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
namespace AIFH_Vol1.Core.General.Fns
{
    /// <summary>
    /// A function that implements a radial basis function (RBF).
    /// </summary>
    public interface IFnRBF : IFn
    {
        /**
 * 
 *
 * @param dimension 
 * @return 
 */

        /// <summary>
        /// Get the center for the specified dimension.
        /// </summary>
        /// <param name="dimension">The dimension.</param>
        /// <returns>The center.</returns>
        double GetCenter(int dimension);

        /// <summary>
        /// Set the center for the specified dimension. 
        /// </summary>
        /// <param name="dimension">The dimension.</param>
        /// <param name="value">The value to set the center.</param>
        void SetCenter(int dimension, double value);

        
        /// <summary>
        /// The dimension count.
        /// </summary>
        int Dimensions { get;  }

        /// <summary>
        /// The width.
        /// </summary>
        double Width { get; set; }

    }
}
