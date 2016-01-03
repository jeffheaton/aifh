using System;
using System.Collections.Generic;
using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Train.Error;

namespace AIFH_Vol3_Core.Core.ANN.Train
{
    /// <summary>
    ///     This class supports several variants of the backpropagation training algorithm for neural networks.  By default,
    ///     this class will perform a stochastic gradient descent(SGD) train with a mini-batch of 500.  The cross-entropy
    ///     error function is used, along with Nesterov momentum.L1 & L2 regularization can also be used.
    ///     With backpropagation is it important to choose a good learning rate and momentum.  If the learning rate is too high
    ///     your network will not converge, and may become unstable with weights going to NaN.Too small a learning rate will
    ///     take a considerable amount of time to train.
    ///     Nesterov, Y. (2004). Introductory lectures on convex optimization(Vol. 87). Springer Science & Business Media.
    ///     Sutskever, Ilya, et al. "On the importance of initialization and momentum in deep learning." Proceedings of the
    ///     30th international conference on machine learning (ICML-13). 2013.
    /// </summary>
    public class BackPropagation : IGradientCalcOwner, ILearningMethod
    {
        /// <summary>
        ///     The last error calculation.
        /// </summary>
        private readonly IErrorCalculation _errorCalc = new ErrorCalculationMSE();

        /// <summary>
        ///     Gradient calculation utility.
        /// </summary>
        private readonly GradientCalc _gradients;

        /// <summary>
        ///     The weight deltas from the last iteration.
        /// </summary>
        private readonly double[] _lastDelta;

        /// <summary>
        ///     The current error.
        /// </summary>
        private double _currentError = 1.0;

        /// <summary>
        ///     If we are doing non-stochastic batches, this keeps track of where we were in the
        ///     training set elements.
        /// </summary>
        private int _currentIndex;

        /// <summary>
        ///     The network to train.
        /// </summary>
        private readonly BasicNetwork _network;

        /// <summary>
        ///     The training set.
        /// </summary>
        private readonly IList<BasicData> _training;

        /// <summary>
        ///     Construct the backpropagation trainer.
        /// </summary>
        /// <param name="theNetwork">The network to train.</param>
        /// <param name="theTraining">The training data to use.</param>
        /// <param name="theLearningRate">The learning rate.  Can be changed as training runs.</param>
        /// <param name="theMomentum">The momentum.  Can be changed as training runs.</param>
        public BackPropagation(BasicNetwork theNetwork, IList<BasicData> theTraining, double theLearningRate,
            double theMomentum)
        {
            BatchSize = 500;
            Stochastic = new MersenneTwisterGenerateRandom();
            NesterovUpdate = true;
            _network = theNetwork;
            _training = theTraining;
            LearningRate = theLearningRate;
            Momentum = theMomentum;
            _gradients = new GradientCalc(_network, new CrossEntropyErrorFunction(), this);
            _lastDelta = new double[theNetwork.Weights.Length];
        }

        /// <summary>
        ///     The learning rate.
        /// </summary>
        public double LearningRate { get; set; }

        /// <summary>
        ///     The momentum.
        /// </summary>
        public double Momentum { get; set; }

        /// <summary>
        ///     The batch size, set to zero for full batch training.
        /// </summary>
        public int BatchSize { get; set; }

        /// <summary>
        ///     Should we use stochastic gradient descent (SGD)?  If so, this holds the random number
        ///     generator.If we do not desire SGD, set this value to null.
        /// </summary>
        private IGenerateRandom Stochastic { get; set; }

        /// <summary>
        ///     Should nesterov update be used?
        /// </summary>
        public bool NesterovUpdate { get; set; }

        /// <inheritdoc />
        public bool IsOnlineTraining
        {
            get { return BatchSize != 0 && (BatchSize < _training.Count); }
        }

        /// <summary>
        ///     L1 regularization weighting, 0.0 for none.
        /// </summary>
        public double L1 { get; set; }

        /// <summary>
        ///     L2 regularization weighting, 0.0 for none.
        /// </summary>
        public double L2 { get; set; }

        /// <inheritdoc />
        public void Iteration()
        {
            _network.NetworkTraining = true;

            // alert the layers that a new batch is starting.
            foreach (var layer in _network.Layers)
            {
                layer.TrainingBatch(Stochastic);
            }

            // begin the iteration
            _gradients.Reset();
            _errorCalc.Clear();

            var iterationSize = BatchSize == 0
                ? _training.Count
                : Math.Min(BatchSize, _training.Count);


            for (var i = 0; i < iterationSize; i++)
            {
                BasicData element;

                if (IsOnlineTraining)
                {
                    if (Stochastic != null)
                    {
                        var stochasticIndex = Stochastic.NextInt(0, _training.Count);
                        element = _training[stochasticIndex];
                    }
                    else
                    {
                        element = _training[_currentIndex++];
                    }
                }
                else
                {
                    element = _training[i];
                }
                _gradients.Process(_errorCalc, element.Input, element.Ideal);
            }

            if (_currentIndex > _training.Count || BatchSize == 0)
            {
                _currentIndex = 0;
            }

            _currentError = _errorCalc.Calculate();

            for (var i = 0; i < _network.Weights.Length; i++)
            {
                double delta;

                if (NesterovUpdate)
                {
                    var prevNesterov = _lastDelta[i];

                    _lastDelta[i] = Momentum*prevNesterov
                                    + _gradients.Gradients[i]*LearningRate;
                    delta = Momentum*prevNesterov - (1 + Momentum)*_lastDelta[i];
                }
                else
                {
                    delta = _gradients.Gradients[i]*-LearningRate + _lastDelta[i]*Momentum;
                    _lastDelta[i] = delta;
                }

                _network.Weights[i] += delta;
            }
            _network.NetworkTraining = false;
        }

        /// <inheritdoc />
        public double LastError
        {
            get { return _currentError; }
        }

        /// <inheritdoc />
        public bool Done
        {
            get { return false; }
        }

        /// <inheritdoc />
        public string Status
        {
            get { return ""; }
        }

        /// <inheritdoc />
        public void FinishTraining()
        {
        }
    }
}