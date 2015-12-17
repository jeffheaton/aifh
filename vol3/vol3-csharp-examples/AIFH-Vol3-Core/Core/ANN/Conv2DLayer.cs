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
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     A 2D convolution layer.
    ///     LeCun, Y., Bottou, L., Bengio, Y., & Haffner, P. (1998). Gradient-based learning applied to document recognition.
    ///     Proceedings of the IEEE, 86(11), 2278-2324.
    /// </summary>
    public class Conv2DLayer : WeightedLayer
    {
        /// <summary>
        ///     The number of filters (output depth).
        /// </summary>
        private readonly int _numFilters;

        /// <summary>
        ///     The input depth.
        /// </summary>
        private int _inDepth;

        /// <summary>
        ///     The output columns.
        /// </summary>
        private double _outColumns;

        /// <summary>
        ///     The output rows.
        /// </summary>
        private double _outRows;

        /// <summary>
        ///     Construct a 2D convolution layer.
        /// </summary>
        /// <param name="theActivation">The activation function.</param>
        /// <param name="theNumFilters">The number of filters.</param>
        /// <param name="theFilterRows">The rows in each filter.</param>
        /// <param name="theFilterColumns">The columns in each filter.</param>
        public Conv2DLayer(IActivationFunction theActivation, int theNumFilters, int theFilterRows, int theFilterColumns)
        {
            Activation = theActivation;
            FilterRows = theFilterRows;
            FilterColumns = theFilterColumns;
            _numFilters = theNumFilters;
        }

        /// <inheritdoc />
        public override int WeightDepthUnit
        {
            get
            {
                var previousLayer = Owner.GetPreviousLayer(this);
                return previousLayer.NeuronDepthUnit*NeuronDepthUnit;
            }
        }

        /// <inheritdoc />
        public override int NeuronDepthUnit
        {
            get { return FilterColumns*FilterRows; }
        }

        /// <inheritdoc />
        public override int Count
        {
            get { return FilterRows*FilterColumns*_numFilters; }
        }

        /// <inheritdoc />
        public override int TotalCount
        {
            get { return Count + 1; }
        }

        /// <summary>
        ///     Conv2D layers always have bias.
        /// </summary>
        public override bool HasBias
        {
            get { return true; }
        }

        /// <inheritdoc />
        public override int[] DimensionCounts
        {
            get { return new[] {FilterColumns, FilterRows, _numFilters}; }
        }

        /// <summary>
        ///     The amount of padding.
        /// </summary>
        public int Padding { get; set; }

        /// <summary>
        ///     The stride.
        /// </summary>
        public int Stride { get; set; }

        /// <summary>
        ///     The number of columns in each filter.
        /// </summary>
        public int FilterColumns { get; set; }

        /// <summary>
        ///     The number of rows in each filter.
        /// </summary>
        public int FilterRows { get; set; }

        /// <inheritdoc />
        public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts)
        {
            FinalizeStructure(theOwner, theLayerIndex, counts);

            var prevLayer = LayerIndex > 0 ? Owner.Layers[LayerIndex - 1] : null;
            var nextLayer = LayerIndex < Owner.Layers.Count - 1 ? Owner.Layers[LayerIndex + 1] : null;

            if (prevLayer == null)
            {
                throw new AIFHError("Conv2DLayer must have a previous layer (cannot be used as the input layer).");
            }

            var inColumns = prevLayer.DimensionCounts[0];
            var inRows = prevLayer.DimensionCounts[1];
            _inDepth = prevLayer.DimensionCounts[2];

            _outColumns = Math.Floor((double) (inColumns + Padding*2 - FilterRows)/Stride + 1);
            _outRows = Math.Floor((double) (inRows + Padding*2 - FilterColumns)/Stride + 1);
        }

        /// <inheritdoc />
        public override void ComputeLayer()
        {
            var prev = Owner.GetPreviousLayer(this);
            int fromCount;

            if (prev is Conv2DLayer)
            {
                fromCount = 1 + ((Conv2DLayer) prev).FilterRows*((Conv2DLayer) prev).FilterRows;
            }
            else if (prev.DimensionCounts.Length == 3)
            {
                fromCount = prev.DimensionCounts[0]*prev.DimensionCounts[1] + 1;
            }
            else
            {
                fromCount = prev.Count;
            }

            // Calculate the output for each filter (depth).
            for (var dOutput = 0; dOutput < _numFilters; dOutput++)
            {
                for (var dInput = 0; dInput < _inDepth; dInput++)
                {
                    ComputeLayer(dInput, dOutput, fromCount, FilterColumns*FilterRows);
                }
            }
        }

        /// <inheritdoc />
        public override void ComputeGradient(GradientCalc calc)
        {
            var prev = Owner.GetPreviousLayer(this);
            var fromLayerSize = prev.TotalCount;
            var toLayerSize = Count;

            // Calculate the output for each filter (depth).
            for (var dOutput = 0; dOutput < _numFilters; dOutput++)
            {
                for (var dInput = 0; dInput < _inDepth; dInput++)
                {
                    ComputeGradient(calc, 0, 0, fromLayerSize, toLayerSize);
                }
            }
        }

        /// <inheritdoc />
        public override void TrainingBatch(IGenerateRandom rnd)
        {
            // nothing to do
        }

        /// <inheritdoc />
        public override bool IsActive(int i)
        {
            return true;
        }
    }
}