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

using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     A dropout layer.  This layer behaves just like a reugular fully-connected layer once the network is trained.
    ///     However, during training some of the neurons will "drop out" of the layer.This greatly reduces overfitting.
    ///     Srivastava, N., Hinton, G., Krizhevsky, A., Sutskever, I., & Salakhutdinov, R. (2014). Dropout: A simple way to
    ///     prevent neural networks from overfitting.The Journal of Machine Learning Research, 15(1), 1929-1958.
    /// </summary>
    public class DropoutLayer : BasicLayer
    {
        /// <summary>
        ///     Which neurons are currently active.
        /// </summary>
        private readonly bool[] _active;

        /// <summary>
        ///     Construct a dropout layer.
        /// </summary>
        /// <param name="theActivation">The activation function.</param>
        /// <param name="theHasBias">True, if this layer has bias.  Dropout layers usually will.</param>
        /// <param name="theCount">The count of neurons.</param>
        /// <param name="theDropout">The dropout probability.</param>
        public DropoutLayer(IActivationFunction theActivation, bool theHasBias, int theCount, double theDropout) :
            base(theActivation, theHasBias, theCount)
        {
            DropoutProbability = theDropout;
            _active = new bool[theCount];
            for (var i = 0; i < _active.Length; i++)
            {
                _active[i] = true;
            }
        }

        /// <summary>
        ///     The probability of a neuron dropping out.
        /// </summary>
        public double DropoutProbability { get; set; }

        /// <summary>
        ///     The neurons in this layer that are currently active.
        /// </summary>
        public bool[] Active
        {
            get { return _active; }
        }

        /// <inheritdoc />
        public override void TrainingBatch(IGenerateRandom rnd)
        {
            for (var i = 0; i < _active.Length; i++)
            {
                _active[i] = rnd.NextDouble() > DropoutProbability;
            }
        }

        /// <inheritdoc />
        public override bool IsActive(int i)
        {
            if (Owner.IsNetworkTraining)
            {
                if (i < _active.Length)
                {
                    return _active[i];
                }
                return true; // bias always active.
            }
            return true;
        }
    }
}