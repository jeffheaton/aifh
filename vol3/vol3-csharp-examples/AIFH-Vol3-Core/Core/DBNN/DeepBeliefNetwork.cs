using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    /// A deep belief neural network.
    ///
    /// References:
    /// http://deeplearning.net/software/theano/
    /// https://github.com/yusugomori/DeepLearning
    /// http://en.wikipedia.org/wiki/Deep_learning
    /// </summary>
    public class DeepBeliefNetwork : IRegressionAlgorithm
    {
        /// <summary>
        /// The hidden layers of the neural network.
        /// </summary>
        private HiddenLayer[] _layers;

        /// <summary>
        /// The restricted boltzmann machines for the neural network, one for leach layer.
        /// </summary>
        private RestrictedBoltzmannMachine[] _rbm;

        /// <summary>
        /// The output layer for the neural network.
        /// </summary>
        private DeepLayer _outputLayer;

        /// <summary>
        /// The random number generator to use.
        /// </summary>
        public IGenerateRandom Random { get; set; }

        /// <summary>
        /// Construct a deep belief neural network. 
        /// </summary>
        /// <param name="inputCount">The input count.</param>
        /// <param name="hidden">The counts for the hidden layers.</param>
        /// <param name="outputCount">The output neuron count.</param>
        public DeepBeliefNetwork(int inputCount, int[] hidden, int outputCount)
        {
            int inputSize;

            _layers = new HiddenLayer[hidden.Length];
            _rbm = new RestrictedBoltzmannMachine[hidden.Length];

            for (int i = 0; i < _rbm.Length; i++)
            {
                if (i == 0)
                {
                    inputSize = inputCount;
                }
                else
                {
                    inputSize = hidden[i - 1];
                }

                _layers[i] = new HiddenLayer(this, inputSize, hidden[i]);

                _rbm[i] = new RestrictedBoltzmannMachine(_layers[i]);
            }

            _outputLayer = new DeepLayer(this, hidden[_layers.Length - 1], outputCount);
            Random = new MersenneTwisterGenerateRandom();
        }

        /// <summary>
        /// Randomize the weights of the neural network.
        /// </summary>
        public void Reset()
        {
            for (int i = 0; i < _rbm.Length; i++)
            {

                HiddenLayer layer = _layers[i];

                double a = 1.0 / layer.InputCount;

                for (int j = 0; j < layer.OutputCount; j++)
                {
                    for (int k = 0; k < layer.InputCount; k++)
                    {
                        layer.Weights[j][k] = Random.NextDouble(-a, a);
                    }
                }
            }
        }

        /// <summary>
        /// The sigmoid/logistic function, used by the output layer. 
        /// </summary>
        /// <param name="x">The input.</param>
        /// <returns>The output.</returns>
        public static double Sigmoid(double x)
        {
            return 1.0 / (1.0 + Math.Exp(-x));
        }

        /// <summary>
        /// The layers of the neural network.
        /// </summary>
        /// <returns></returns>
        public HiddenLayer[] Layers
        {
            get
            {
                return _layers;
            }
        }

        /// <summary>
        /// The restricted Boltzmann machines.
        /// </summary>
        public RestrictedBoltzmannMachine[] RBMLayers
        {
            get
            {
                return _rbm;
            }
        }

        /// <summary>
        /// The input count.
        /// </summary>
        public int InputCount
        {
            get
            {
                return _layers[0].InputCount;
            }
        }

        /// <summary>
        /// The output (logistic) layer.
        /// </summary>
        public DeepLayer LogLayer
        {
            get
            {
                return _outputLayer;
            }
        }


        /// <summary>
        /// The number of output neurons.
        /// </summary>
        public int OutputCount
        {
            get
            {
                return _outputLayer.OutputCount;
            }
        }

        /// <summary>
        /// Classify the input data into the list of probabilities of each class. 
        /// </summary>
        /// <param name="input">The input.</param>
        /// <returns>An array that contains the probabilities of each class.</returns>
        public double[] ComputeRegression(double[] input)
        {

            double[] result = new double[OutputCount];
            double[] layerInput = new double[0];
            double[] prevLayerInput = new double[InputCount];

            Array.Copy(input, prevLayerInput, InputCount);

            double output;

            for (int i = 0; i < _layers.Length; i++)
            {
                layerInput = new double[_layers[i].OutputCount];

                for (int k = 0; k < _layers[i].OutputCount; k++)
                {
                    output = 0.0;

                    for (int j = 0; j < _layers[i].InputCount; j++)
                    {
                        output += _layers[i].Weights[k][j] * prevLayerInput[j];
                    }
                    output += _layers[i].Bias[k];
                    layerInput[k] = Sigmoid(output);
                }

                if (i < _layers.Length - 1)
                {
                    prevLayerInput = new double[_layers[i].OutputCount];
                    Array.Copy(layerInput, 0, prevLayerInput, 0, _layers[i].OutputCount);
                }
            }

            for (int i = 0; i < _outputLayer.OutputCount; i++)
            {
                result[i] = 0;
                for (int j = 0; j < _outputLayer.InputCount; j++)
                {
                    result[i] += _outputLayer.Weights[i][j] * layerInput[j];
                }
                result[i] += _outputLayer.Bias[i];
            }

            _outputLayer.Softmax(result);
            return result;
        }

        /// <inheritdoc/>
        public double[] LongTermMemory
        {
            get
            {
                throw new AIFHError("Can't access DBM memory as array.");
            }
        }
    }
}
