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

using System.Text;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     Base class for all layers (used with BasicNetwork) that have weights.
    /// </summary>
    public abstract class WeightedLayer : ILayer
    {
        /// <summary>
        ///     The activation function.
        /// </summary>
        private IActivationFunction _activation;

        /// <summary>
        ///     The layer index.
        /// </summary>
        private int _layerIndex;

        /// <summary>
        ///     The index to this layer's neurons.
        /// </summary>
        private int _neuronIndex;

        /// <summary>
        ///     The network that owns this layer.
        /// </summary>
        private BasicNetwork _owner;

        /// <summary>
        ///     The index to this layer's weights.
        /// </summary>
        private int _weightIndex;

        /// <inheritdoc />
        public void FinalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts)
        {
            _owner = theOwner;
            _layerIndex = theLayerIndex;

            var prevLayer = _layerIndex > 0 ? _owner.Layers[_layerIndex - 1] : null;
            var nextLayer = _layerIndex < _owner.Layers.Count - 1 ? _owner.Layers[_layerIndex + 1] : null;

            var tc = TotalCount;
            counts.AddNeuronCount(tc);

            if (prevLayer != null)
            {
                counts.AddWeightCount(Count*prevLayer.TotalCount);
            }

            int weightIndex, layerIndex;
            if (theLayerIndex == _owner.Layers.Count - 1)
            {
                weightIndex = 0;
                layerIndex = 0;
            }
            else
            {
                weightIndex = nextLayer.WeightIndex
                              + TotalCount*nextLayer.Count;
                layerIndex = nextLayer.NeuronIndex + nextLayer.TotalCount;
            }

            _neuronIndex = layerIndex;
            _weightIndex = weightIndex;
        }


        /// <inheritdoc />
        public int WeightIndex
        {
            get { return _weightIndex; }
        }

        /// <inheritdoc />
        public int NeuronIndex
        {
            get { return _neuronIndex; }
        }

        /// <inheritdoc />
        public int LayerIndex
        {
            get { return _layerIndex; }
        }

        /// <inheritdoc />
        public BasicNetwork Owner
        {
            get { return _owner; }
        }


        /// <inheritdoc />
        public IActivationFunction Activation
        {
            get { return _activation; }
            set { _activation = value; }
        }

        /// <inheritdoc />
        public abstract int Count { get; }

        /// <inheritdoc />
        public abstract int TotalCount { get; }

        /// <inheritdoc />
        public abstract bool HasBias { get; }

        /// <inheritdoc />
        public abstract int[] DimensionCounts { get; }

        /// <inheritdoc />
        public abstract int WeightDepthUnit { get; }

        /// <inheritdoc />
        public abstract int NeuronDepthUnit { get; }

        /// <inheritdoc />
        public abstract void ComputeLayer();

        /// <inheritdoc />
        public abstract void ComputeGradient(GradientCalc calc);

        /// <inheritdoc />
        public abstract void TrainingBatch(IGenerateRandom rnd);

        /// <inheritdoc />
        public abstract bool IsActive(int i);

        /// <summary>
        ///     Compute a layer.
        /// </summary>
        /// <param name="inputOffset">The offset to the input for this layer.</param>
        /// <param name="outputOffset">The offset to the output from this layer.</param>
        /// <param name="fromCount">The count of from neurons.</param>
        /// <param name="toCount">The count of to neurons.</param>
        public void ComputeLayer(int inputOffset, int outputOffset, int fromCount, int toCount)
        {
            var prev = Owner.GetPreviousLayer(this);
            var weights = Owner.Weights;
            var weightSize = WeightDepthUnit;
            var outputSize = NeuronDepthUnit;

            var index = WeightIndex + inputOffset*weightSize;

            // weight values
            for (var ix = 0; ix < toCount; ix++)
            {
                var x = NeuronIndex + ix + outputOffset*outputSize;
                double sum = 0;

                for (var y = 0; y < fromCount; y++)
                {
                    if (prev.IsActive(ix) && IsActive(y))
                    {
                        sum += weights[index]*Owner.LayerOutput[prev.NeuronIndex + y];
                    }
                    index++;
                }
                Owner.LayerSums[x] += sum;
                Owner.LayerOutput[x] += sum;
            }

            Activation.ActivationFunction(
                Owner.LayerOutput, NeuronIndex, toCount);
        }

        /// <summary>
        ///     Compute gradients for this layer.
        /// </summary>
        /// <param name="calc">The gradient calculator.</param>
        /// <param name="inputOffset">The input offset.</param>
        /// <param name="outputOffset">The output offset.</param>
        /// <param name="fromLayerSize">The from layer size.</param>
        /// <param name="toLayerSize">The to layer size.</param>
        public void ComputeGradient(GradientCalc calc, int inputOffset, int outputOffset, int fromLayerSize,
            int toLayerSize)
        {
            var prev = Owner.GetPreviousLayer(this);
            var fromLayerIndex = prev.NeuronIndex;
            var toLayerIndex = NeuronIndex;
            var weightSize = WeightDepthUnit;
            var outputSize = NeuronDepthUnit;


            var index = WeightIndex + weightSize*inputOffset; // this.weightIndex[currentLevel];
            var activation = Activation;

            // handle weights
            // array references are made method local to avoid one indirection
            var layerDelta = calc.LayerDelta;
            var weights = Owner.Weights;
            var layerOutput = Owner.LayerOutput;
            var layerSums = Owner.LayerSums;
            var y = fromLayerIndex;
            for (var yi = 0; yi < fromLayerSize; yi++)
            {
                var output = layerOutput[y];
                double sum = 0;

                var wi = index + yi;

                for (var xi = 0; xi < toLayerSize; xi++, wi += fromLayerSize)
                {
                    var x = xi + toLayerIndex;

                    if (prev.IsActive(yi) && IsActive(xi))
                        calc.Gradients[wi] += -(output*layerDelta[x]);
                    sum += weights[wi]*layerDelta[x];
                }
                layerDelta[y] = sum
                                *activation.DerivativeFunction(layerSums[y], layerOutput[y]);

                y++;
            }
        }

        /// <inheritdoc />
        public override string ToString()
        {
            var result = new StringBuilder();
            result.Append("[");
            result.Append(GetType().Name);
            result.Append(",count=").Append(Count);
            result.Append(",weightIndex=").Append(WeightIndex);
            result.Append(",neuronIndex=").Append(NeuronIndex);

            result.Append("]");
            return result.ToString();
        }
    }
}