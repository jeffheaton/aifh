using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3_Core.Core.ANN.Train.Error;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Train
{
    /// <summary>
    /// One problem with the backpropagation algorithm is that the magnitude of the
    /// partial derivative is usually too large or too small.Further, the learning
    /// rate is a single value for the entire neural network. The resilient
    /// propagation learning algorithm uses a special update value(similar to the
    /// learning rate) for every neuron connection.Further these update values are
    /// automatically determined, unlike the learning rate of the backpropagation
    /// algorithm.
    ///
    /// There are a total of three parameters that must be provided to the resilient
    /// training algorithm. Defaults are provided for each, and in nearly all cases,
    /// these defaults are acceptable.This makes the resilient propagation algorithm
    /// one of the easiest and most efficient training algorithms available.
    ///
    /// It is also important to note that RPROP does not work well with online training.
    /// You should always use a batch size bigger than one.Typically the larger the better.
    /// By default a batch size of zero is used, zero means to include the entire training
    /// set in the batch.
    ///
    /// The optional parameters are:
    ///
    /// zeroTolerance - How close to zero can a number be to be considered zero. The
    /// default is 0.00000000000000001.
    ///
    /// initialUpdate - What are the initial update values for each matrix value.The
    /// default is 0.1.
    ///
    /// maxStep - What is the largest amount that the update values can step.The
    /// default is 50.xw
    /// </summary>
    public class ResilientPropagation : IGradientCalcOwner, ILearningMethod
    {
        /// <summary>
        /// The network to train.
        /// </summary>
        private BasicNetwork _network;

        /// <summary>
        /// The training data.
        /// </summary>
        private IList<BasicData> _training;

        /// <summary>
        /// The gradients.
        /// </summary>
        private readonly GradientCalc _gradients;

        /// <summary>
        /// The weight delta from the last training iteration.
        /// </summary>
        private double[] _lastDelta;

        /// <summary>
        /// The gradients from the last training iteration.
        /// </summary>
        private double[] _lastGradients;

        /// <summary>
        /// The error calculation method to use.
        /// </summary>
        private IErrorCalculation _errorCalc = new ErrorCalculationMSE();

        /**
         * The current error.
         */
        private double _currentError = 1.0;


        /// <summary>
        /// L1 regularization weighting, 0.0 for none.
        /// </summary>
        public double L1 { get; set; }

        /// <summary>
        /// L2 regularization weighting, 0.0 for none.
        /// </summary>
        public double L2 { get; set; }

        /// <summary>
        /// The current update values.
        /// </summary>
        private double[] _updateValues;

        /// <summary>
        /// The POSITIVE ETA value. This is specified by the resilient propagation
        /// algorithm.This is the percentage by which the deltas are increased by if
        /// the partial derivative is greater than zero.
        /// </summary>
        public const double POSITIVE_ETA = 1.2;

        /// <summary>
        /// The NEGATIVE ETA value. This is specified by the resilient propagation
        /// algorithm.This is the percentage by which the deltas are increased by if
        /// the partial derivative is less than zero.
        /// </summary>
        public const double NEGATIVE_ETA = 0.5;

        /// <summary>
        /// The minimum delta value for a weight matrix value.
        /// </summary>
        public const double DELTA_MIN = 1e-6;

        /// <summary>
        /// The starting update for a delta.
        /// </summary>
        public const double DEFAULT_INITIAL_UPDATE = 0.1;

        /// <summary>
        /// The maximum amount a delta can reach.
        /// </summary>
        public const double DEFAULT_MAX_STEP = 50;

        /// <summary>
        /// Construct a RPROP trainer.
        /// </summary>
        /// <param name="theNetwork">The network.</param>
        /// <param name="theTraining">The training data.</param>
        public ResilientPropagation(BasicNetwork theNetwork, IList<BasicData> theTraining)
        {
            _network = theNetwork;
            _training = theTraining;
            _gradients = new GradientCalc(_network, new CrossEntropyErrorFunction(), this);
            _lastDelta = new double[theNetwork.Weights.Length];
            _updateValues = new double[theNetwork.Weights.Length];
            _lastGradients = new double[theNetwork.Weights.Length];

            for (int i = 0; i < _updateValues.Length; i++)
            {
                _updateValues[i] = ResilientPropagation.DEFAULT_INITIAL_UPDATE;
            }
        }

        /// <inheritdoc/>
        public void Iteration()
        {
            _gradients.Reset();
            _errorCalc.Clear();

            // Calculate gradients for entire training set, RPROP does not do online.
            foreach (BasicData element in _training)
            {
                _gradients.Process(_errorCalc, element.Input, element.Ideal);
            }
            _currentError = _errorCalc.Calculate();

            // Apply the gradients according to the RPROP algorithm.
            for (int i = 0; i < _gradients.Gradients.Length; i++)
            {
                double delta = CalculateWeightDelta(_gradients.Gradients, _lastGradients, i);
                _lastGradients[i] = _gradients.Gradients[i];
                _lastDelta[i] = delta;
                _network.Weights[i] += delta;
            }


        }
        

        /// <summary>
        /// Calculate the change in weights. 
        /// </summary>
        /// <param name="gradients">The gradients.</param>
        /// <param name="lastGradient">The last graidents.</param>
        /// <param name="index">The weight currently being updated.</param>
        /// <returns>The weight change.</returns>
        public double CalculateWeightDelta(double[] gradients,
                                       double[] lastGradient, int index)
        {
            // multiply the current and previous gradient, and take the
            // sign. We want to see if the gradient has changed its sign.
            int change = (int)Math.Sign(gradients[index] * lastGradient[index]);
            double weightChange = 0;

            // if the gradient has retained its sign, then we increase the
            // delta so that it will converge faster
            if (change > 0)
            {
                double delta = _updateValues[index]
                        * ResilientPropagation.POSITIVE_ETA;
                delta = Math.Min(delta, ResilientPropagation.DEFAULT_MAX_STEP);
                weightChange = -Math.Sign(gradients[index]) * delta;
                _updateValues[index] = delta;
                lastGradient[index] = gradients[index];
            }
            else if (change < 0)
            {
                // if change<0, then the sign has changed, and the last
                // delta was too big
                double delta = _updateValues[index]
                        * ResilientPropagation.NEGATIVE_ETA;
                delta = Math.Max(delta, ResilientPropagation.DELTA_MIN);
                _updateValues[index] = delta;
                weightChange = -_lastDelta[index];
                // set the previous gradent to zero so that there will be no
                // adjustment the next iteration
                lastGradient[index] = 0;
            }
            else if (change == 0)
            {
                // if change==0 then there is no change to the delta
                double delta = _updateValues[index];
                weightChange = -Math.Sign(gradients[index]) * delta;
                lastGradient[index] = gradients[index];
            }

            // apply the weight change, if any
            return weightChange;
        }

        /// <summary>
        /// The error from the last training iteration.
        /// </summary>
        public double LastError
        {
            get
            {
                return _currentError;
            }
        }

        /// <summary>
        /// True, if we are done learning.  Not all learning algorithms know when they are done, in this case
        /// false is always returned.
        /// </summary>
        public bool Done
        {
            get
            {
                return false;
            }
        }

        /// <summary>
        /// A string that indicates the status of training.
        /// </summary>
        public String Status
        {
            get
            {
                return "";
            }
        }

        /// <summary>
        /// Should be called after the last iteration to make sure training completes any final tasks.
        /// </summary>
        public void FinishTraining()
        {

        }
    }
}
