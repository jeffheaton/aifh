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
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     A fully connected weight layer in a neural network.  This layer type is used for input and output layers.
    ///     This layer type is also one of several hidden layer types available.
    /// </summary>
    public class BasicLayer : WeightedLayer
    {
        /// <summary>
        ///     The neuron count.
        /// </summary>
        private readonly int[] _count;

        /// <summary>
        ///     True if this layer has bias.
        /// </summary>
        private readonly bool _hasBias;


        /// <summary>
        ///     Do not use this constructor.  This was added to support serialization.
        /// </summary>
        public BasicLayer()
        {
        }

        /// <summary>
        ///     Construct a multi-dimensional input layer.  This layer is usually used in conjunction with a
        ///     convolutional neural network(CNN/LeNET).
        /// </summary>
        /// <param name="ActivationFunction">The activation function.</param>
        /// <param name="theHasBias">True, if this layer has bias, input layers will usually have bias, others will not.</param>
        /// <param name="theCount">The number of neurons in each dimension.</param>
        public BasicLayer(IActivationFunction theActivation, bool theHasBias, int[] theCount)
        {
            if (theCount.Length != 1 && theCount.Length != 3)
            {
                throw new AIFHError("The number of dimensions must be 1 or 3.");
            }
            Activation = theActivation;
            _hasBias = theHasBias;
            _count = theCount;
        }

        /// <summary>
        ///     Construct a single dimension layer, this is usually used for non-convolutional neural networks.
        /// </summary>
        /// <param name="activation">The activation function.  All layers, except input will have activation functions.</param>
        /// <param name="theHasBias">True, if this layer has a bias, all layers except the output have bias.</param>
        /// <param name="theCount">The neuron count.</param>
        public BasicLayer(IActivationFunction activation, bool theHasBias, int theCount)
            : this(activation, theHasBias, new[] {theCount})
        {
        }


        /// <summary>
        ///     The count.
        /// </summary>
        public override int Count
        {
            get
            {
                var product = 1;
                for (var i = 0; i < _count.Length; i++)
                {
                    product *= _count[i];
                }
                return product;
            }
        }

        /// <summary>
        ///     The total number of neurons on this layer, includes context, bias
        ///     and regular.
        /// </summary>
        public override int TotalCount
        {
            get { return Count + (HasBias ? 1 : 0); }
        }

        /// <summary>
        ///     The bias.
        /// </summary>
        public override bool HasBias
        {
            get { return _hasBias; }
        }

        /// <inheritdoc />
        public override int[] DimensionCounts
        {
            get { return _count; }
        }

        /// <inheritdoc />
        public override int WeightDepthUnit
        {
            get
            {
                var previousLayer = Owner.GetPreviousLayer(this);
                int prevCount;
                if (previousLayer is Conv2DLayer)
                {
                    prevCount = ((Conv2DLayer) previousLayer).FilterColumns*
                                ((Conv2DLayer) previousLayer).FilterRows;
                }
                else
                {
                    if (previousLayer.DimensionCounts.Length == 1)
                    {
                        prevCount = previousLayer.Count;
                    }
                    else
                    {
                        prevCount = previousLayer.DimensionCounts[0]*previousLayer.DimensionCounts[1];
                    }
                }
                if (previousLayer.HasBias)
                {
                    prevCount++;
                }


                return prevCount*NeuronDepthUnit;
            }
        }

        /// <inheritdoc />
        public override int NeuronDepthUnit
        {
            get
            {
                if (_count.Length == 3)
                {
                    return _count[0]*_count[1];
                }
                return _count[0];
            }
        }


        /// <inheritdoc />
        public override void ComputeLayer()
        {
            var prev = Owner.GetPreviousLayer(this);
            ComputeLayer(0, 0, prev.TotalCount, Count);
        }

        /// <inheritdoc />
        public override void ComputeGradient(GradientCalc calc)
        {
            var prev = Owner.GetPreviousLayer(this);
            var fromLayerSize = prev.TotalCount;
            var toLayerSize = Count;
            ComputeGradient(calc, 0, 0, fromLayerSize, toLayerSize);
        }

        /// <inheritdoc />
        public override void TrainingBatch(IGenerateRandom rnd)
        {
            // Nothing needs to be done!
        }

        /// <inheritdoc />
        public override bool IsActive(int i)
        {
            return true;
        }
    }
}