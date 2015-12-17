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

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    ///     A layer for a deep belief neural network.
    /// </summary>
    public class DeepLayer
    {
        /// <summary>
        ///     The biases.
        /// </summary>
        private readonly double[] _bias;

        /// <summary>
        ///     The network that owns this layer.
        /// </summary>
        private readonly DeepBeliefNetwork _owner;

        /// <summary>
        ///     The weights of this layer.
        /// </summary>
        private readonly double[][] _weights;

        /// <summary>
        ///     Construct the layer.
        /// </summary>
        /// <param name="theOwner">The network that owns this layer.</param>
        /// <param name="inputCount">The input count for this layer.</param>
        /// <param name="outputCount">The output count for this layer.</param>
        public DeepLayer(DeepBeliefNetwork theOwner, int inputCount, int outputCount)
        {
            _weights = AIFH.Alloc2D<double>(outputCount, inputCount);
            _bias = new double[outputCount];
            _owner = theOwner;
        }

        /// <summary>
        ///     The input count.
        /// </summary>
        public virtual int InputCount
        {
            get { return _weights[0].Length; }
        }

        /// <summary>
        ///     The output count.
        /// </summary>
        public virtual int OutputCount
        {
            get { return _weights.Length; }
        }

        /// <summary>
        ///     The weights.
        /// </summary>
        public double[][] Weights
        {
            get { return _weights; }
        }

        /// <summary>
        ///     The biases.
        /// </summary>
        public double[] Bias
        {
            get { return _bias; }
        }

        /// <summary>
        ///     The network.
        /// </summary>
        public DeepBeliefNetwork Owner
        {
            get { return _owner; }
        }

        /// <summary>
        ///     Used to calculate the softmax for this layer.
        /// </summary>
        /// <param name="x">The input to the softmax.</param>
        public void Softmax(double[] x)
        {
            var max = 0.0;
            var sum = 0.0;

            foreach (var aX in x)
            {
                if (max < aX)
                {
                    max = aX;
                }
            }

            for (var i = 0; i < x.Length; i++)
            {
                x[i] = Math.Exp(x[i] - max);
                sum += x[i];
            }

            for (var i = 0; i < x.Length; i++)
            {
                x[i] /= sum;
            }
        }
    }
}