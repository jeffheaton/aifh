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
using AIFH_Vol3.Core.General.Fns;

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    ///     Implements a multi-dimensional RBF neighborhood function.
    /// </summary>
    public class NeighborhoodRBF : INeighborhoodFunction
    {
        /// <summary>
        ///     The size of each dimension.
        /// </summary>
        private readonly int[] _size;

        public readonly double SQ75 = Math.Sqrt(0.75);

        /// <summary>
        ///     The displacement of each dimension, when mapping the dimensions
        ///     to a 1d array.
        /// </summary>
        private int[] _displacement;

        private bool _hexagon;

        private readonly double[] _params;

        /// <summary>
        ///     The radial basis function to use.
        /// </summary>
        private readonly IFnRBF _rbf;

        /// <summary>
        ///     Construct a 2d neighborhood function based on the sizes for the
        ///     x and y dimensions.
        /// </summary>
        /// <param name="type">The RBF type to use.</param>
        /// <param name="x">The size of the x-dimension.</param>
        /// <param name="y">The size of the y-dimension.</param>
        public NeighborhoodRBF(RBFEnum type, int x, int y)
        {
            var size = new int[2];
            size[0] = x;
            size[1] = y;

            _params = new double[3];

            switch (type)
            {
                case RBFEnum.Gaussian:
                    _rbf = new GaussianFunction(2, _params, 0);
                    break;
                case RBFEnum.InverseMultiquadric:
                    _rbf = new InverseMultiquadricFunction(2, _params, 0);
                    break;
                case RBFEnum.Multiquadric:
                    _rbf = new MultiquadricFunction(2, _params, 0);
                    break;
                case RBFEnum.MexicanHat:
                    _rbf = new MexicanHatFunction(2, _params, 0);
                    break;
            }

            _rbf.Width = 1;

            _size = size;

            CalculateDisplacement();
        }

        /// <summary>
        ///     Construct a multi-dimensional neighborhood function.
        /// </summary>
        /// <param name="size">The sizes of each dimension.</param>
        /// <param name="type">The RBF type to use.</param>
        public NeighborhoodRBF(int[] size, RBFEnum type)
        {
            _params = new double[size.Length];

            switch (type)
            {
                case RBFEnum.Gaussian:
                    _rbf = new GaussianFunction(size.Length, _params, 0);
                    break;
                case RBFEnum.InverseMultiquadric:
                    _rbf = new InverseMultiquadricFunction(size.Length, _params, 0);
                    break;
                case RBFEnum.Multiquadric:
                    _rbf = new MultiquadricFunction(size.Length, _params, 0);
                    break;
                case RBFEnum.MexicanHat:
                    _rbf = new MexicanHatFunction(size.Length, _params, 0);
                    break;
            }
            _size = size;
            CalculateDisplacement();
        }

        /// <summary>
        ///     The RBF to use.
        /// </summary>
        public IFnRBF RBF
        {
            get { return _rbf; }
        }

        /// <summary>
        ///     Sets or determines if this RBF is laid out as a hexigon.
        /// </summary>
        public bool Hexagon
        {
            get { return _hexagon; }
            set
            {
                if (value && _size.Length != 2)
                {
                    throw new AIFHError("Hexagon lattice can only be used in two dimensions.");
                }
                _hexagon = value;
            }
        }

        /// <summary>
        ///     Calculate the value for the multi RBF function.
        /// </summary>
        /// <param name="currentNeuron">The current neuron.</param>
        /// <param name="bestNeuron">The best neuron.</param>
        /// <returns>
        ///     A percent that determines the amount of training the current
        ///     neuron should get.Usually 100% when it is the bestNeuron.
        /// </returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            var vector = new double[_displacement.Length];
            var vectorCurrent = TranslateCoordinates(currentNeuron);
            var vectorBest = TranslateCoordinates(bestNeuron);
            for (var i = 0; i < vectorCurrent.Length; i++)
            {
                vector[i] = vectorCurrent[i] - vectorBest[i];
            }

            if (_hexagon)
            {
                var row = vector[1];
                var col = vector[0];
                double evenIndent = 1;
                var oddIndent = 2.5;
                var indent = row%2 == 1 ? oddIndent : evenIndent;

                vector[1] = (int) (SQ75 + row*SQ75);
                vector[0] = (int) (indent + 3*col);
            }


            return _rbf.Evaluate(vector);
        }

        /// <summary>
        ///     The radius.
        /// </summary>
        public double Radius
        {
            get { return _rbf.Width; }
            set { _rbf.Width = value; }
        }

        /// <summary>
        ///     Calculate all of the displacement values.
        /// </summary>
        private void CalculateDisplacement()
        {
            _displacement = new int[_size.Length];
            for (var i = 0; i < _size.Length; i++)
            {
                int value;

                if (i == 0)
                {
                    value = 0;
                }
                else if (i == 1)
                {
                    value = _size[0];
                }
                else
                {
                    value = _displacement[i - 1]*_size[i - 1];
                }

                _displacement[i] = value;
            }
        }

        /// <summary>
        ///     Translate the specified index into a set of multi-dimensional
        ///     coordinates that represent the same index.This is how the
        ///     multi-dimensional coordinates are translated into a one dimensional
        ///     index for the input neurons.
        /// </summary>
        /// <param name="index">The index to translate.</param>
        /// <returns>The multi-dimensional coordinates.</returns>
        private int[] TranslateCoordinates(int index)
        {
            var result = new int[_displacement.Length];
            var countingIndex = index;

            for (var i = _displacement.Length - 1; i >= 0; i--)
            {
                int value;
                if (_displacement[i] > 0)
                {
                    value = countingIndex/_displacement[i];
                }
                else
                {
                    value = countingIndex;
                }

                countingIndex -= _displacement[i]*value;
                result[i] = value;
            }

            return result;
        }
    }
}