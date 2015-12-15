using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Error;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train.Error;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Train
{
    /// <summary>
    /// A utility class used to help calculate the gradient of the error function for neural networks.
    /// </summary>
    public class GradientCalc
    {
        /// <summary>
        /// The network to train.
        /// </summary>
        private readonly BasicNetwork _network;

        /// <summary>
        /// The actual values from the neural network.
        /// </summary>
        private readonly double[] _actual;

        /// <summary>
        /// The deltas for each layer.
        /// </summary>
        private readonly double[] _layerDelta;

        /// <summary>
        /// The output from each layer.
        /// </summary>
        private readonly double[] _layerOutput;

        /// <summary>
        /// The sums.
        /// </summary>
        private readonly double[] _layerSums;

        /// <summary>
        /// The gradients.
        /// </summary>
        private readonly double[] _gradients;

        /// <summary>
        /// The weights and thresholds.
        /// </summary>
        private readonly double[] _weights;

        /// <summary>
        /// The owner of the gradient calculation.
        /// </summary>
        private readonly IGradientCalcOwner _owner;

        /// <summary>
        /// The error function to use.
        /// </summary>
        private readonly IErrorFunction errorFunction;
        
        /// <summary>
        /// Construct the gradient calculation class.
        /// </summary>
        /// <param name="theNetwork">The network to use.</param>
        /// <param name="ef">The error function to use.</param>
        /// <param name="theOwner">The owner (usually a trainer).</param>
        public GradientCalc(BasicNetwork theNetwork,
                        IErrorFunction ef, IGradientCalcOwner theOwner)
        {
            _network = theNetwork;
            this.errorFunction = ef;

            _layerDelta = new double[_network.LayerOutput.Length];
            _gradients = new double[_network.Weights.Length];
            _actual = new double[_network.OutputCount];

            _weights = _network.Weights;
            _layerOutput = _network.LayerOutput;
            _layerSums = _network.LayerSums;
            _owner = theOwner;
        }

        /// <summary>
        /// The network being processed.
        /// </summary>
        public BasicNetwork Network
        {
            get
            {
                return _network;
            }
        }

        /// <summary>
        /// The weights for this network.
        /// </summary>
        public double[] Weights
        {
            get
            {
                return _weights;
            }
        }
        
        /// <summary>
        /// Process one training set element. 
        /// </summary>
        /// <param name="errorCalc">The error calculation to use.</param>
        /// <param name="input">The network input.</param>
        /// <param name="ideal">The ideal values.</param>
        public void Process(IErrorCalculation errorCalc, double[] input, double[] ideal)
        {
            _network.Compute(input, _actual);

            errorCalc.UpdateError(_actual, ideal, 1.0);

            // Calculate error for the output layer.
            int outputLayerIndex = _network.Layers.Count - 1;
            IActivationFunction outputActivation = _network.Layers[outputLayerIndex].Activation;
            this.errorFunction.CalculateError(
                    outputActivation, _layerSums, _layerOutput,
                    ideal, _actual, _layerDelta, 0, 1.0);

            // Apply regularization, if requested.
            if (_owner.L1 > AIFH.DefaultPrecision
                    || _owner.L1 > AIFH.DefaultPrecision)
            {
                double[] lp = new double[2];
                CalculateRegularizationPenalty(lp);
                for (int i = 0; i < _actual.Length; i++)
                {
                    double p = (lp[0] * _owner.L1) + (lp[1] * _owner.L2);
                    _layerDelta[i] += p;
                }
            }

            // Propagate backwards (chain rule from calculus).
            for (int i = _network.Layers.Count - 1; i > 0; i--)
            {
                ILayer layer = _network.Layers[i];
                layer.ComputeGradient(this);
            }
        }


        /// <summary>
        /// Reset all gradients to zero.
        /// </summary>
        public void Reset()
        {
            for (int i = 0; i < _gradients.Length; i++)
            {
                _gradients[i] = 0;
            }
        }


        /// <summary>
        /// The gradients.
        /// </summary>
        public double[] Gradients
        {
            get
            {
                return _gradients;
            }
        }

        /// <summary>
        /// Calculate the regularization penalty.
        /// </summary>
        /// <param name="l">The penalty.</param>
        public void CalculateRegularizationPenalty(double[] l)
        {
            for (int i = 0; i < _network.Layers.Count - 1; i++)
            {
                LayerRegularizationPenalty(i, l);
            }
        }

        /// <summary>
        /// Apply a regularization penalty, such as that from L1/L2 regularization. 
        /// </summary>
        /// <param name="fromLayer">The from layer.</param>
        /// <param name="l">The penalty.</param>
        public void LayerRegularizationPenalty(int fromLayer, double[] l)
        {
            int fromCount = _network.GetLayerTotalNeuronCount(fromLayer);
            int toCount = _network.Layers[fromLayer + 1].Count;

            for (int fromNeuron = 0; fromNeuron < fromCount; fromNeuron++)
            {
                for (int toNeuron = 0; toNeuron < toCount; toNeuron++)
                {
                    double w = _network.GetWeight(fromLayer, fromNeuron, toNeuron);
                    l[0] += Math.Abs(w);
                    l[1] += w * w;
                }
            }
        }

        /// <summary>
        /// The layer deltas used to calculate the gradient.
        /// </summary>
        public double[] LayerDelta
        {
            get
            {
                return _layerDelta;
            }
        }
    }
}
