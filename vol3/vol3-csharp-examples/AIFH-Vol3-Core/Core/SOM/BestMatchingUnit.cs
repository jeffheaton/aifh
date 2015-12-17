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
using AIFH_Vol3.Core;
using MathNet.Numerics.LinearAlgebra.Double;

namespace AIFH_Vol3_Core.Core.SOM
{
    /// <summary>
    ///     The "Best Matching Unit" or BMU is a very important concept in the training
    ///     for a SOM.The BMU is the output neuron that has weight connections to the
    ///     input neurons that most closely match the current input vector.This neuron
    ///     (and its "neighborhood") are the neurons that will receive training.
    ///     This class also tracks the worst distance(of all BMU's). This gives some
    ///     indication of how well the network is trained, and thus becomes the "error"
    ///     of the entire network.
    /// </summary>
    public class BestMatchingUnit
    {
        /// <summary>
        ///     The owner of this class.
        /// </summary>
        private readonly SelfOrganizingMap _som;

        /// <summary>
        ///     What is the worst BMU distance so far, this becomes the error for the
        ///     entire SOM.
        /// </summary>
        private double _worstDistance;

        /// <summary>
        ///     Construct a BestMatchingUnit class.  The training class must be provided.
        /// </summary>
        /// <param name="som">The SOM to evaluate.</param>
        public BestMatchingUnit(SelfOrganizingMap som)
        {
            _som = som;
        }

        /// <summary>
        ///     What is the worst BMU distance so far, this becomes the error
        ///     for the entire SOM.
        /// </summary>
        public double WorstDistance
        {
            get { return _worstDistance; }
        }

        /// <summary>
        ///     Calculate the best matching unit (BMU). This is the output neuron that
        ///     has the lowest Euclidean distance to the input vector.
        /// </summary>
        /// <param name="input">The input vector.</param>
        /// <returns>The output neuron number that is the BMU.</returns>
        public int CalculateBMU(double[] input)
        {
            var result = 0;

            if (input.Length > _som.InputCount)
            {
                throw new AIFHError("Can't train SOM with input size of " + _som.InputCount
                                    + " with input data of count "
                                    + input.Length);
            }

            // Track the lowest distance so far.
            var lowestDistance = double.PositiveInfinity;

            for (var i = 0; i < _som.OutputCount; i++)
            {
                var distance = CalculateEuclideanDistance(_som.Weights, input,
                    i);

                // Track the lowest distance, this is the BMU.
                if (distance < lowestDistance)
                {
                    lowestDistance = distance;
                    result = i;
                }
            }

            // Track the worst distance, this is the error for the entire network.
            if (lowestDistance > _worstDistance)
            {
                _worstDistance = lowestDistance;
            }

            return result;
        }

        /// <summary>
        ///     Calculate the Euclidean distance for the specified output neuron and the
        ///     input vector.This is the square root of the squares of the differences
        ///     between the weight and input vectors.
        /// </summary>
        /// <param name="matrix">The matrix to get the weights from.</param>
        /// <param name="input">The input vector.</param>
        /// <param name="outputNeuron">The neuron we are calculating the distance for.</param>
        /// <returns>The Euclidean distance.</returns>
        public double CalculateEuclideanDistance(Matrix matrix,
            double[] input, int outputNeuron)
        {
            double result = 0;

            // Loop over all input data.
            for (var i = 0; i < input.Length; i++)
            {
                var diff = input[i]
                           - matrix[outputNeuron, i];
                result += diff*diff;
            }
            return Math.Sqrt(result);
        }

        /// <summary>
        ///     Reset the "worst distance" back to a minimum value.  This should be
        ///     called for each training iteration.
        /// </summary>
        public void Reset()
        {
            _worstDistance = double.PositiveInfinity;
        }
    }
}