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
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.ACO
{
    /// <summary>
    /// An individual ant for continuous ACO.
    /// </summary>
    public class ContinuousAnt: IComparable<ContinuousAnt>
    {
        /// <summary>
        /// The score for this ant.
        /// </summary>
        public double Score { get; set; }

        /// <summary>
        /// The parameters for this ant.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        /// True, if this ant should minimize.  This value should be the same for all ants.
        /// </summary>
        private bool _shouldMinimize;

        /// <summary>
        /// The constructor. 
        /// </summary>
        /// <param name="n">The number of parameters (dimensions).</param>
        /// <param name="theShouldMinimize">True, if we are minimizing.</param>
        public ContinuousAnt(int n, bool theShouldMinimize)
        {
            _params = new double[n];
            _shouldMinimize = theShouldMinimize;
        }

        /// <summary>
        /// The parameters for this ant.
        /// </summary>
        public double[] Params
        {
            get
            {
                return _params;
            }
        }

        /// <inheritdoc/>
        public int CompareTo(ContinuousAnt o)
        {
            if (_shouldMinimize)
            {
                return Score.CompareTo(o.Score);
            }
            else
            {
                return o.Score.CompareTo(Score);
            }
        }
    }
}
