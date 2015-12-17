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
using AIFH_Vol3.Core.General.Fns;

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    ///     A neighborhood function based on an RBF function.
    /// </summary>
    public class NeighborhoodRBF1D : INeighborhoodFunction
    {
        private readonly double[] _params = new double[2];

        /// <summary>
        ///     The radial basis function (RBF) to use to calculate the training falloff
        ///     from the best neuron.
        /// </summary>
        private readonly IFnRBF _radial;

        /// <summary>
        ///     Construct the neighborhood function with the specified radial function.
        ///     Generally this will be a Gaussian function but any RBF should do.
        /// </summary>
        /// <param name="radial">The radial basis function to use.</param>
        public NeighborhoodRBF1D(IFnRBF radial)
        {
            _radial = radial;
        }

        /// <summary>
        ///     Construct a 1d neighborhood function.
        /// </summary>
        /// <param name="type">The RBF type to use.</param>
        public NeighborhoodRBF1D(RBFEnum type)
        {
            switch (type)
            {
                case RBFEnum.Gaussian:
                    _radial = new GaussianFunction(1, _params, 0);
                    break;
                case RBFEnum.InverseMultiquadric:
                    _radial = new InverseMultiquadricFunction(1, _params, 0);
                    break;
                case RBFEnum.Multiquadric:
                    _radial = new MultiquadricFunction(1, _params, 0);
                    break;
                case RBFEnum.MexicanHat:
                    _radial = new MexicanHatFunction(1, _params, 0);
                    break;
                default:
                    throw new AIFHError("Unknown RBF type: " + type);
            }

            _radial.Width = 1.0;
        }

        /// <summary>
        ///     Determine how much the current neuron should be affected by training
        ///     based on its proximity to the winning neuron.
        /// </summary>
        /// <param name="currentNeuron"> THe current neuron being evaluated.</param>
        /// <param name="bestNeuron">The winning neuron.</param>
        /// <returns>The ratio for this neuron's adjustment.</returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            var d = new double[1];
            d[0] = currentNeuron - bestNeuron;
            return _radial.Evaluate(d);
        }

        /// <summary>
        ///     The radius.
        /// </summary>
        public double Radius
        {
            get { return _radial.Width; }
            set { _radial.Width = value; }
        }
    }
}