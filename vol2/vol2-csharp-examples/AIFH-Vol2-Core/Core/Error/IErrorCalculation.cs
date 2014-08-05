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
namespace AIFH_Vol2.Core.Error
{
    /// <summary>
    /// An error calculation metric calculates the difference between two vector sets.  One vector set will be the ideal
    /// expected output from a Machine Learning Algorithm.  The other vector set is the actual output.  Training a
    /// Machine Learning Algorithm typically involves minimizing this error.
    ///
    /// Error calculation metrics are very similar to distance metrics.  However, an error calculation metric operates over
    /// a set of vectors, whereas a distance metric operates over just two vectors.
    /// </summary>
    public interface IErrorCalculation
    {
        /// <summary>
        /// Called to update for each number that should be checked.
        /// </summary>
        /// <param name="actual">The actual number.</param>
        /// <param name="ideal">The ideal number.</param>
        /// <param name="significance">The significance, 0 none, 1.0 full.</param>
        void UpdateError(double[] actual, double[] ideal, double significance);

        /// <summary>
        /// Update the error with single values.s 
        /// </summary>
        /// <param name="actual">The actual value.</param>
        /// <param name="ideal">The ideal value.</param>
        void UpdateError(double actual, double ideal);

        /// <summary>
        /// Calculate the error with MSE. 
        /// </summary>
        /// <returns>The current error for the neural network.</returns>
        double Calculate();

        /// <summary>
        /// Clear the error calculation and start over.
        /// </summary>
        void Clear();

        /// <summary>
        /// The total size of the set (vector size times number of vectors).
        /// </summary>
        int SetSize { get; }

        /// <summary>
        /// Create a new instance of this object.
        /// </summary>
        /// <returns>A new instance of this object.</returns>
        IErrorCalculation Create();
    }
}
