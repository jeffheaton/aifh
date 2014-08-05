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
using System.Linq;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2_MergePhysics.Universe
{
    /// <summary>
    ///     An individual universe cell.  Each cell can have multiple properties.  Typically there are three properties,
    ///     that represent the three RGB components.
    /// </summary>
    public class UniverseCell
    {
        /// <summary>
        ///     The properties for this cell.
        /// </summary>
        private readonly double[] _prop;


        /// <summary>
        ///     Construct a universe cell with the specified number of properties.
        /// </summary>
        /// <param name="theSize">The number of properties.</param>
        public UniverseCell(int theSize)
        {
            _prop = new double[theSize];
        }

        /// <summary>
        ///     Get an average of the properties.
        /// </summary>
        public double Avg
        {
            get
            {
                double result = _prop.Sum();
                return result/_prop.Length;
            }
        }

        /// <summary>
        ///     The property array.
        /// </summary>
        public double[] Data
        {
            get { return _prop; }
        }

        /// <summary>
        ///     The number of properties.
        /// </summary>
        public int Count
        {
            get { return _prop.Length; }
        }

        /// <summary>
        ///     Add the specified value to the specified property.
        /// </summary>
        /// <param name="i">The property index.</param>
        /// <param name="d">The other value to add.</param>
        public void Add(int i, double d)
        {
            _prop[i] += d;
        }

        /// <summary>
        ///     Add the properties of another cell to this one.
        /// </summary>
        /// <param name="otherCell">The other cell.</param>
        public void Add(UniverseCell otherCell)
        {
            for (int i = 0; i < _prop.Length; i++)
            {
                _prop[i] += otherCell.Data[i];
            }
        }

        /// <summary>
        ///     Randomize the properties between (-1,1).
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        public void Randomize(IGenerateRandom rnd)
        {
            for (int i = 0; i < _prop.Length; i++)
            {
                _prop[i] = rnd.NextDouble(-1, 1);
            }
        }

        /// <summary>
        ///     Set this cell's properties to another cell.
        /// </summary>
        /// <param name="otherCell">The other cell.</param>
        public void Set(UniverseCell otherCell)
        {
            for (int i = 0; i < _prop.Length; i++)
            {
                _prop[i] = otherCell.Data[i];
            }
        }
    }
}
