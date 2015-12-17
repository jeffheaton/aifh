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

using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Distance;
using AIFH_Vol3.Core.Randomize;
using MathNet.Numerics.LinearAlgebra.Double;

namespace AIFH_Vol3_Core.Core.SOM
{
    public class SelfOrganizingMap
    {
        /// <summary>
        ///     The weights.
        /// </summary>
        private readonly Matrix _weights;

        /// <summary>
        ///     Distance calculation.
        /// </summary>
        private readonly ICalculateDistance calcDist = new EuclideanDistance();

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="inputCount">Number of input neurons</param>
        /// <param name="outputCount">Number of output neurons</param>
        public SelfOrganizingMap(int inputCount, int outputCount)
        {
            _weights = DenseMatrix.Create(outputCount, inputCount, 0);
        }

        /// <inheritdoc />
        public int InputCount
        {
            get { return _weights.ColumnCount; }
        }

        /// <inheritdoc />
        public int OutputCount
        {
            get { return _weights.RowCount; }
        }

        /// <summary>
        ///     The weights.
        /// </summary>
        public Matrix Weights
        {
            get { return _weights; }
        }


        public double CalculateError(double[][] data)
        {
            var bmu = new BestMatchingUnit(this);

            bmu.Reset();

            // Determine the BMU for each training element.
            foreach (var pair in data)
            {
                var input = pair;
                bmu.CalculateBMU(input);
            }

            // update the error
            return bmu.WorstDistance/100.0;
        }

        /// <inheritdoc />
        public int Classify(double[] input)
        {
            if (input.Length > InputCount)
            {
                throw new AIFHError(
                    "Can't classify SOM with input size of " + InputCount
                    + " with input data of count " + input.Length);
            }

            var minDist = double.PositiveInfinity;
            var result = -1;

            var rowWeights = _weights.ToRowArrays();

            for (var i = 0; i < OutputCount; i++)
            {
                var dist = calcDist.Calculate(input, rowWeights[i]);
                if (dist < minDist)
                {
                    minDist = dist;
                    result = i;
                }
            }

            return result;
        }


        public void Reset(IGenerateRandom rnd)
        {
            for (var i = 0; i < _weights.RowCount; i++)
            {
                for (var j = 0; j < _weights.ColumnCount; j++)
                {
                    _weights[i, j] = rnd.NextDouble(-1, 1);
                }
            }
        }

        public void Reset()
        {
            Reset(new MersenneTwisterGenerateRandom());
        }

        /// <summary>
        ///     An alias for the classify method, kept for compatibility
        ///     with earlier versions of Encog.
        /// </summary>
        /// <param name="input">The input pattern.</param>
        /// <returns>The winning neuron.</returns>
        public int Winner(double[] input)
        {
            return Classify(input);
        }
    }
}