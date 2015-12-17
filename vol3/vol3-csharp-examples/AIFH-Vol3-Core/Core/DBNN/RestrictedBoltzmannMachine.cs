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

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    ///     Restricted Boltzmann machine, for deep belief neural network.
    /// </summary>
    public class RestrictedBoltzmannMachine
    {
        /// <summary>
        ///     The hidden bias.
        /// </summary>
        private readonly double[] _hBias;

        /// <summary>
        ///     The hidden layer that this RBM corresponds to.
        /// </summary>
        private readonly HiddenLayer _layer;

        /// <summary>
        ///     The neural network.
        /// </summary>
        private readonly DeepBeliefNetwork _owner;

        /// <summary>
        ///     The visable bias.
        /// </summary>
        private readonly double[] _vBias;

        /// <summary>
        ///     Construct restricted Boltzmann machine.
        /// </summary>
        /// <param name="theLayer">The layer that this RBM works with.</param>
        public RestrictedBoltzmannMachine(HiddenLayer theLayer)
        {
            _layer = theLayer;
            _owner = theLayer.Owner;
            _hBias = _layer.Bias;
            _vBias = new double[VisibleCount];
        }

        /// <summary>
        ///     The visable neuron count.
        /// </summary>
        public int VisibleCount
        {
            get { return _layer.InputCount; }
        }

        /// <summary>
        ///     The hidden neuron count.
        /// </summary>
        public int HiddenCount
        {
            get { return _layer.OutputCount; }
        }

        /// <summary>
        ///     The hidden layer that goes with this RBM.
        /// </summary>
        public HiddenLayer Layer
        {
            get { return _layer; }
        }

        /// <summary>
        ///     Hidden biases.
        /// </summary>
        public double[] BiasH
        {
            get { return _hBias; }
        }

        /// <summary>
        ///     Visable biases.
        /// </summary>
        public double[] BiasV
        {
            get { return _vBias; }
        }

        /// <summary>
        ///     The network owner.
        /// </summary>
        public DeepBeliefNetwork Owner
        {
            get { return _owner; }
        }

        /// <summary>
        ///     Sample a bimodal value with the specified probability.  Returns the count of sampled true values.
        /// </summary>
        /// <param name="n">The number of values to sample.</param>
        /// <param name="p">The probability of true.</param>
        /// <returns>The count of true values.</returns>
        public int binomial(int n, double p)
        {
            if (p < 0 || p > 1) return 0;

            var c = 0;
            double r;

            for (var i = 0; i < n; i++)
            {
                r = _owner.Random.NextDouble();
                if (r < p) c++;
            }

            return c;
        }

        /// <summary>
        ///     Sigmoid function.
        /// </summary>
        /// <param name="x">The input.</param>
        /// <returns>The output.</returns>
        public static double Sigmoid(double x)
        {
            return 1.0/(1.0 + Math.Exp(-x));
        }
    }
}