using System;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    /// Not currently used.  Will soon support maxpooling layers.
    /// </summary>
    public class MaxPoolLayer: ILayer
    {
        /// <summary>
        /// Construct a max pooling layer.
        /// </summary>
        /// <param name="count">The counts.</param>
        public MaxPoolLayer(int[] count)
        {

        }

        /// <inheritdoc/>
        public IActivationFunction Activation
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int Count
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int[] DimensionCounts
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public bool HasBias
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int LayerIndex
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int NeuronDepthUnit
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int NeuronIndex
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public BasicNetwork Owner
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int TotalCount
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int WeightDepthUnit
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public int WeightIndex
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        /// <inheritdoc/>
        public void ComputeGradient(GradientCalc calc)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        public void ComputeLayer()
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        public void FinalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        public bool IsActive(int i)
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        public void TrainingBatch(IGenerateRandom rnd)
        {
            throw new NotImplementedException();
        }
    }
}
