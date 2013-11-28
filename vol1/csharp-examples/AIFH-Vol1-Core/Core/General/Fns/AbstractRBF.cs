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

using System.Text;

namespace AIFH_Vol1.Core.General.Fns
{
    /// <summary>
    /// Provides the basics for an RBF function.  RBF functions take their "parameters" from a vector 
    /// (and starting index). This allows many RBF's to be "stacked" together in a single vector.  
    /// RBF parameters are: a single width and a vector of centers.  Therefor the size required to store 
    /// one RBF is (dimensions + 1).  There is no peak parameter, the peak is assumed to be 1.
    /// </summary>
    public abstract class AbstractRBF : IFnRBF
    {
        /// <summary>
        /// The parameter vector.  Holds the RBF width and centers.  This vector may hold multiple RBF's.
        /// </summary>
        private readonly double[] _prms;

        /// <summary>
        /// The index to the widths.
        /// </summary>
        private readonly int _indexWidth;

        /// <summary>
        /// The index to the centers.
        /// </summary>
        private readonly int _indexCenters;

        /// <summary>
        /// The dimensions.
        /// </summary>
        private readonly int _dimensions;

        /// <summary>
        /// Construct the RBF. Each RBF will require space equal to (dimensions + 1) in the params vector. 
        /// </summary>
        /// <param name="theDimensions">The number of dimensions.</param>
        /// <param name="theParams">A vector to hold the paramaters.</param>
        /// <param name="theIndex">The index into the params vector.  You can store multiple RBF's in a vector.</param>
        public AbstractRBF(int theDimensions, double[] theParams, int theIndex)
        {
            _dimensions = theDimensions;
            _prms = theParams;
            _indexWidth = theIndex;
            _indexCenters = theIndex + 1;
        }

        /// <inheritdoc/>
        public double GetCenter(int dimension)
        {
            return _prms[_indexCenters + dimension];
        }

        /// <inheritdoc/>
        public int Dimensions
        {
            get
            {
                return _dimensions;
            }
        }

        /// <inheritdoc/>
        public double Width
        {
            get
            {
                return _prms[_indexWidth];
            }
            set
            {
                _prms[_indexWidth] = value;
            }
        }


        /// <inheritdoc/>
        public override string ToString()
        {
            var result = new StringBuilder();
            result.Append("[");
            result.Append(GetType().Name);
            result.Append(":width=");
            result.Append(Width.ToString("0.00"));
            result.Append(",center=");
            for (var i = 0; i < _dimensions; i++)
            {
                if (i > 0)
                {
                    result.Append(",");
                }
                result.Append(_prms[_indexCenters + i].ToString("0.00"));
            }

            result.Append("]");
            return result.ToString();
        }

        /// <inheritdoc />
        public void SetCenter(int dimension, double value)
        {
            _prms[_indexCenters + dimension] = value;
        }

        /// <inheritdoc/>
        abstract public double Evaluate(double[] x);
    }
}
