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
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     Not currently used.  Will soon support maxpooling layers.
    /// </summary>
    public class MaxPoolLayer : ILayer
    {
        /// <summary>
        ///     Construct a max pooling layer.
        /// </summary>
        /// <param name="count">The counts.</param>
        public MaxPoolLayer(int[] count)
        {
        }

        /// <inheritdoc />
        public IActivationFunction Activation
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int Count
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int[] DimensionCounts
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public bool HasBias
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int LayerIndex
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int NeuronDepthUnit
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int NeuronIndex
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public BasicNetwork Owner
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int TotalCount
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int WeightDepthUnit
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public int WeightIndex
        {
            get { throw new NotImplementedException(); }
        }

        /// <inheritdoc />
        public void ComputeGradient(GradientCalc calc)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc />
        public void ComputeLayer()
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc />
        public void FinalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc />
        public bool IsActive(int i)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc />
        public void TrainingBatch(IGenerateRandom rnd)
        {
            throw new NotImplementedException();
        }
    }
}