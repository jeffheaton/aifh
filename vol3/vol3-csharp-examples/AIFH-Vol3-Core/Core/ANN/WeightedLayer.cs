using AIFH_Vol3_Core.Core.ANN.Activation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    /// Base class for all layers (used with BasicNetwork) that have weights.
    /// </summary>
    public abstract class WeightedLayer: ILayer
    {
        /// <summary>
        /// The layer index.
        /// </summary>
        private int _layerIndex;

        /// <summary>
        /// The network that owns this layer.
        /// </summary>
        private BasicNetwork _owner;

        /// <summary>
        /// The index to this layer's weights.
        /// </summary>
        private int _weightIndex;

        /// <summary>
        /// The index to this layer's neurons.
        /// </summary>
        private int _neuronIndex;

        /// <summary>
        /// The activation function.
        /// </summary>
        private IActivationFunction _activation;

        /// <inheritdoc/>
        public void FinalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts)
        {
            _owner = theOwner;
            _layerIndex = theLayerIndex;

            ILayer prevLayer = (_layerIndex > 0) ? _owner.Layers[_layerIndex - 1] : null;
            ILayer nextLayer = (_layerIndex < _owner.Layers.Count - 1) ? _owner.Layers[_layerIndex + 1] : null;

            int tc = TotalCount;
            counts.AddNeuronCount(tc);

            if (prevLayer != null)
            {
                counts.AddWeightCount(Count * prevLayer.TotalCount);
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
                        + (TotalCount * nextLayer.Count);
                layerIndex = nextLayer.NeuronIndex + nextLayer.TotalCount;
                
            }

            _neuronIndex = layerIndex;
            _weightIndex = weightIndex;
        }
        
        /// <summary>
        /// Compute a layer. 
        /// </summary>
        /// <param name="inputOffset">The offset to the input for this layer.</param>
        /// <param name="outputOffset">The offset to the output from this layer.</param>
        /// <param name="fromCount">The count of from neurons.</param>
        /// <param name="toCount">The count of to neurons.</param>
        public void ComputeLayer(int inputOffset, int outputOffset, int fromCount, int toCount)
        {
            ILayer prev = Owner.GetPreviousLayer(this);
            double[] weights = Owner.Weights;
            int weightSize = WeightDepthUnit;
            int outputSize = NeuronDepthUnit;

            int index = WeightIndex + (inputOffset * weightSize);

            // weight values
            for (int ix = 0; ix < toCount; ix++)
            {
                int x = NeuronIndex + ix + (outputOffset * outputSize);
                double sum = 0;

                for (int y = 0; y < fromCount; y++)
                {
                    if (prev.IsActive(ix) && IsActive(y))
                    {
                        sum += weights[index] * Owner.LayerOutput[prev.NeuronIndex + y];
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
        /// Compute gradients for this layer. 
        /// </summary>
        /// <param name="calc">The gradient calculator.</param>
        /// <param name="inputOffset">The input offset.</param>
        /// <param name="outputOffset">The output offset.</param>
        /// <param name="fromLayerSize">The from layer size.</param>
        /// <param name="toLayerSize">The to layer size.</param>
        public void ComputeGradient(GradientCalc calc, int inputOffset, int outputOffset, int fromLayerSize, int toLayerSize)
        {
            ILayer prev = Owner.GetPreviousLayer(this);
            int fromLayerIndex = prev.NeuronIndex;
            int toLayerIndex = NeuronIndex;
            int weightSize = WeightDepthUnit;
            int outputSize = NeuronDepthUnit;


            int index = WeightIndex + (weightSize * inputOffset); // this.weightIndex[currentLevel];
            IActivationFunction activation = Activation;

            // handle weights
            // array references are made method local to avoid one indirection
            double[] layerDelta = calc.LayerDelta;
            double[] weights = this.Owner.Weights;
            double[] layerOutput = Owner.LayerOutput;
            double[] layerSums = Owner.LayerSums;
            int y = fromLayerIndex;
            for (int yi = 0; yi < fromLayerSize; yi++)
            {
                double output = layerOutput[y];
                double sum = 0;

                int wi = index + yi;

                for (int xi = 0; xi < toLayerSize; xi++, wi += fromLayerSize)
                {
                    int x = xi + toLayerIndex;

                    if (prev.IsActive(yi) && IsActive(xi))
                        calc.Gradients[wi] += -(output * layerDelta[x]);
                    sum += weights[wi] * layerDelta[x];
                }
                layerDelta[y] = sum
                        * (activation.DerivativeFunction(layerSums[y], layerOutput[y]));

                y++;
            }
        }


        /// <inheritdoc/>
        public int WeightIndex
        {
            get
            {
                return _weightIndex;
            }
        }

        /// <inheritdoc/>
        public int NeuronIndex
        {
            get
            {
                return _neuronIndex;
            }
        }

        /// <inheritdoc/>
        public int LayerIndex { get { return _layerIndex; } }

        /// <inheritdoc/>
        public BasicNetwork Owner
        { get {
                return _owner;
            }
        }


        /// <inheritdoc/>
        public IActivationFunction Activation
        {
            get
            {
                return _activation;
            }
            set
            {
                _activation = value;
            }
        }

        /// <inheritdoc/>
        public abstract int Count { get; }

        /// <inheritdoc/>
        public abstract int TotalCount { get; }

        /// <inheritdoc/>
        public abstract bool HasBias { get; }

        /// <inheritdoc/>
        public abstract int[] DimensionCounts { get; }

        /// <inheritdoc/>
        public abstract int WeightDepthUnit { get; }

        /// <inheritdoc/>
        public abstract int NeuronDepthUnit { get; }

        /// <inheritdoc/>
        public override String ToString()
        {
            StringBuilder result = new StringBuilder();
            result.Append("[");
            result.Append(this.GetType().Name);
            result.Append(",count=").Append(Count);
            result.Append(",weightIndex=").Append(WeightIndex);
            result.Append(",neuronIndex=").Append(NeuronIndex);

            result.Append("]");
            return result.ToString();
        }

        /// <inheritdoc/>
        public abstract void ComputeLayer();
        /// <inheritdoc/>
        public abstract void ComputeGradient(GradientCalc calc);
        /// <inheritdoc/>
        public abstract void TrainingBatch(IGenerateRandom rnd);
        /// <inheritdoc/>
        public abstract bool IsActive(int i);
    }
}
