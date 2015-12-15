using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    /// A fully connected weight layer in a neural network.  This layer type is used for input and output layers.
    /// This layer type is also one of several hidden layer types available.
    /// </summary>
    public class BasicLayer : WeightedLayer
    {
        /// <summary>
        /// The neuron count.
        /// </summary>
        private int[] _count;

        /// <summary>
        /// True if this layer has bias.
        /// </summary>
        private bool _hasBias;


        /// <summary>
        /// Do not use this constructor.  This was added to support serialization.
        /// </summary>
        public BasicLayer()
        {

        }

        /// <summary>
        /// Construct a multi-dimensional input layer.  This layer is usually used in conjunction with a
        /// convolutional neural network(CNN/LeNET). 
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
        /// Construct a single dimension layer, this is usually used for non-convolutional neural networks. 
        /// </summary>
        /// <param name="activation">The activation function.  All layers, except input will have activation functions.</param>
        /// <param name="theHasBias">True, if this layer has a bias, all layers except the output have bias.</param>
        /// <param name="theCount">The neuron count.</param>
        public BasicLayer(IActivationFunction activation, bool theHasBias, int theCount)
            : this(activation, theHasBias, new int[] { theCount })
        {
        }




        /// <summary>
        /// The count.
        /// </summary>
        public override int Count
        {
            get
            {
                int product = 1;
                for (int i = 0; i < _count.Length; i++)
                {
                    product *= _count[i];
                }
                return product;
            }
        }

        /// <summary>
        /// The total number of neurons on this layer, includes context, bias
        /// and regular.
        /// </summary>
        public override int TotalCount
        {
            get
            {
                return Count + (HasBias ? 1 : 0);
            }
        }

        /// <summary>
        /// The bias.
        /// </summary>
        public override bool HasBias
        {
            get
            {
                return _hasBias;
            }
        }


        /// <inheritdoc/>
        public override void ComputeLayer()
        {
            ILayer prev = Owner.GetPreviousLayer(this);
            ComputeLayer(0, 0, prev.TotalCount, Count);
        }

        /// <inheritdoc/>
        public override void ComputeGradient(GradientCalc calc)
        {
            ILayer prev = Owner.GetPreviousLayer(this);
            int fromLayerSize = prev.TotalCount;
            int toLayerSize = Count;
            ComputeGradient(calc, 0, 0, fromLayerSize, toLayerSize);
        }

        /// <inheritdoc/>
        public override void TrainingBatch(IGenerateRandom rnd)
        {
            // Nothing needs to be done!
        }

        /// <inheritdoc/>
        public override bool IsActive(int i)
        {
            return true;
        }

        /// <inheritdoc/>
        public override int[] DimensionCounts
        {
            get
            {
                return _count;
            }
        }

        /// <inheritdoc/>
        public override int WeightDepthUnit
        {
            get
            {
                ILayer previousLayer = Owner.GetPreviousLayer(this);
                int prevCount;
                if (previousLayer is Conv2DLayer)
                {
                    prevCount = (((Conv2DLayer)previousLayer).FilterColumns *
                            ((Conv2DLayer)previousLayer).FilterRows);
                }
                else
                {
                    if (previousLayer.DimensionCounts.Length == 1)
                    {
                        prevCount = previousLayer.Count;
                    }
                    else
                    {
                        prevCount = previousLayer.DimensionCounts[0] * previousLayer.DimensionCounts[1];
                    }
                }
                if (previousLayer.HasBias)
                {
                    prevCount++;
                }


                return prevCount * NeuronDepthUnit;
            }
        }

        /// <inheritdoc/>
        public override int NeuronDepthUnit
        {
            get
            {
                if (_count.Length == 3)
                {
                    return _count[0] * _count[1];
                }
                else
                {
                    return _count[0];
                }
            }
        }
    }
}
