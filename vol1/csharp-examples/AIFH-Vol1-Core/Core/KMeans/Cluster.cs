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

using System.Collections.Generic;
using System.Text;
using AIFH_Vol1.Core.General;
using AIFH_Vol1.Core.General.Data;

namespace AIFH_Vol1.Core.KMeans
{
    /// <summary>
    /// A cluster of observations. All observations must have the same number of dimensions.
    /// </summary>
    public class Cluster
    {
        /// <summary>
        /// The observations in this cluster.
        /// </summary>
        private readonly IList<BasicData> _observations = new List<BasicData>();

        /// <summary>
        /// The center of these observations.
        /// </summary>
        private readonly double[] _center;

        /// <summary>
        /// Construct a cluster with the specified number of dimensions. 
        /// </summary>
        /// <param name="theDimensions">The number of dimensions.</param>
        public Cluster(int theDimensions)
        {
            _center = new double[theDimensions];
        }


        /// <summary>
        /// Get the number of dimensions. 
        /// </summary>
        public int Dimensions
        {
            get
            {
                return _center.Length;
            }
        }

        /// <summary>
        /// The center of the observations.
        /// </summary>
        public double[] Center
        {
            get
            {
                return _center;
            }
        }

        /// <summary>
        /// The observations in this cluster.
        /// </summary>
        public IList<BasicData> Observations
        {
            get
            {
                return _observations;
            }
        }

        /// <summary>
        /// Calculate the center (or mean) of the observations.
        /// </summary>
        public void CalculateCenter()
        {

            // First, resent the center to zero.
            for (int i = 0; i < _center.Length; i++)
            {
                _center[i] = 0;
            }

            // Now sum up all of the observations to the center.
            foreach (BasicData observation in _observations)
            {
                for (int i = 0; i < _center.Length; i++)
                {
                    _center[i] += observation.Input[i];
                }
            }

            // Divide by the number of observations to get the mean.
            for (int i = 0; i < _center.Length; i++)
            {
                _center[i] /= _observations.Count;
            }
        }

        /// <inheritdoc/>
        public override string ToString()
        {
            var result = new StringBuilder();
            result.Append("[Cluster: dimensions=");
            result.Append(Dimensions);
            result.Append(", observations=");
            result.Append(_observations.Count);
            result.Append(", center=");
            result.Append(VectorUtil.DoubleArrayToString(_center));
            result.Append("]");
            return result.ToString();
        }

    }
}
