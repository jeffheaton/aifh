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
using System.Linq;

namespace AIFH_Vol1.Core.Normalize
{
    /// <summary>
    /// Used to produce an array of activations to classify data into groups. This
    /// class is provided the number of groups, as well as the range that the
    /// activations should fall into.
    /// </summary>
    [Serializable]
    public class Equilateral
    {
        /// <summary>
        /// The minimum number of fields to use equilateral encoding.
        /// </summary>
        public const int MinEq = 3;

        /// <summary>
        /// The matrix of values that was generated.
        /// </summary>
        private readonly double[][] _matrix;

        /// <summary>
        /// Construct an equilateral matrix. 
        /// </summary>
        /// <param name="count">The number of sets, these will be the rows in the matrix.</param>
        /// <param name="low">The high value for the outputs.</param>
        /// <param name="high">The low value for the outputs.</param>
        public Equilateral(int count, double low, double high)
        {
            if (count < MinEq)
            {
                throw new AIFHError("Must have at least three classes.");
            }
            _matrix = Equilat(count, low, high);
        }

        /// <summary>
        /// Decode a set of activations and see which set it has the lowest Euclidean
        /// distance from. 
        /// </summary>
        /// <param name="activations">The output from the neural network.</param>
        /// <returns>The set that these activations were closest too.</returns>
        public int Decode(double[] activations)
        {
            double minValue = Double.PositiveInfinity;
            int minSet = -1;

            for (int i = 0; i < _matrix.Length; i++)
            {
                double dist = GetDistance(activations, i);
                if (dist < minValue)
                {
                    minValue = dist;
                    minSet = i;
                }
            }
            return minSet;
        }

        /// <summary>
        /// Get the activations for the specified set. 
        /// </summary>
        /// <param name="set">The set to determine the activations for.</param>
        /// <returns>The activations for the specified sets.</returns>
        public double[] Encode(int set)
        {
            if (set < 0 || set > _matrix.Length)
            {
                throw new AIFHError("Class out of range for equilateral: " + set);
            }
            return _matrix[set];
        }

        /// <summary>
        /// Called internally to generate the matrix. 
        /// </summary>
        /// <param name="n">The number of sets to generate for.</param>
        /// <param name="low">The high end of the range of values to generate.</param>
        /// <param name="high">The low end of the range of values to generate.</param>
        /// <returns>One row for each set, the columns are the activations for that set.</returns>
        private double[][] Equilat(int n,
                                   double low, double high)
        {
            var result = new double[n][];

            // first, fully allocate the array
            for (int i = 0; i < n; i++)
            {
                result[i] = new double[n - 1];
            }

            // now fill the array with values.
            result[0][0] = -1;
            result[1][0] = 1.0;

            for (int k = 2; k < n; k++)
            {
                // scale the matrix so far
                double r = k;
                double f = Math.Sqrt(r * r - 1.0) / r;
                for (int i = 0; i < k; i++)
                {
                    for (int j = 0; j < k - 1; j++)
                    {
                        result[i][j] *= f;
                    }
                }

                r = -1.0 / r;
                for (int i = 0; i < k; i++)
                {
                    result[i][k - 1] = r;
                }

                for (int i = 0; i < k - 1; i++)
                {
                    result[k][i] = 0.0;
                }
                result[k][k - 1] = 1.0;
            }

            // scale it
            foreach (double[] t in result)
            {
                for (int col = 0; col < result[0].Length; col++)
                {
                    const double min = -1;
                    const double max = 1;
                    t[col] = ((t[col] - min) / (max - min))
                             * (high - low) + low;
                }
            }

            return result;
        }

        /// <summary>
        /// Get the Euclidean distance between the specified data and the set number.
        /// </summary>
        /// <param name="data">The data to check.</param>
        /// <param name="set">The set to check.</param>
        /// <returns>The distance.</returns>
        public double GetDistance(double[] data, int set)
        {
            double result = data.Select((t, i) => Math.Pow(t - _matrix[set][i], 2)).Sum();
            return Math.Sqrt(result);
        }
    }
}
