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
using System.Collections.Generic;
using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3_Core.Core.ANN.Train.Error;

namespace AIFH_Vol3_Core.Core.ANN.Train
{
    /// <summary>
    ///     One problem with the backpropagation algorithm is that the magnitude of the
    ///     partial derivative is usually too large or too small.Further, the learning
    ///     rate is a single value for the entire neural network. The resilient
    ///     propagation learning algorithm uses a special update value(similar to the
    ///     learning rate) for every neuron connection.Further these update values are
    ///     automatically determined, unlike the learning rate of the backpropagation
    ///     algorithm.
    ///     There are a total of three parameters that must be provided to the resilient
    ///     training algorithm. Defaults are provided for each, and in nearly all cases,
    ///     these defaults are acceptable.This makes the resilient propagation algorithm
    ///     one of the easiest and most efficient training algorithms available.
    ///     It is also important to note that RPROP does not work well with online training.
    ///     You should always use a batch size bigger than one.Typically the larger the better.
    ///     By default a batch size of zero is used, zero means to include the entire training
    ///     set in the batch.
    ///     The optional parameters are:
    ///     zeroTolerance - How close to zero can a number be to be considered zero. The
    ///     default is 0.00000000000000001.
    ///     initialUpdate - What are the initial update values for each matrix value.The
    ///     default is 0.1.
    ///     maxStep - What is the largest amount that the update values can step.The
    ///     default is 50.xw
    /// </summary>
    public class ResilientPropagation : IGradientCalcOwner, ILearningMethod
    {
        /// <summary>
        ///     The POSITIVE ETA value. This is specified by the resilient propagation
        ///     algorithm.This is the percentage by which the deltas are increased by if
        ///     the partial derivative is greater than zero.
        /// </summary>
        public const double POSITIVE_ETA = 1.2;

        /// <summary>
        ///     The NEGATIVE ETA value. This is specified by the resilient propagation
        ///     algorithm.This is the percentage by which the deltas are increased by if
        ///     the partial derivative is less than zero.
        /// </summary>
        public const double NEGATIVE_ETA = 0.5;

        /// <summary>
        ///     The minimum delta value for a weight matrix value.
        /// </summary>
        public const double DELTA_MIN = 1e-6;

        /// <summary>
        ///     The starting update for a delta.
        /// </summary>
        public const double DEFAULT_INITIAL_UPDATE = 0.1;

        /// <summary>
        ///     The maximum amount a delta can reach.
        /// </summary>
        public const double DEFAULT_MAX_STEP = 50;

        /// <summary>
        ///     The gradients.
        /// </summary>
        private readonly GradientCalc _gradients;

        /// <summary>
        ///     The error calculation method to use.
        /// </summary>
        private readonly IErrorCalculation _errorCalc = new ErrorCalculationMSE();

        /// <summary>
        ///     The weight delta from the last training iteration.
        /// </summary>
        private readonly double[] _lastDelta;

        /// <summary>
        ///     The gradients from the last training iteration.
        /// </summary>
        private readonly double[] _lastGradients;

        /// <summary>
        ///     The network to train.
        /// </summary>
        private readonly BasicNetwork _network;

        /// <summary>
        ///     The training data.
        /// </summary>
        private readonly IList<BasicData> _training;

        /// <summary>
        ///     The current update values.
        /// </summary>
        private readonly double[] _updateValues;

        /// <summary>
        ///     Construct a RPROP trainer.
        /// </summary>
        /// <param name="theNetwork">The network.</param>
        /// <param name="theTraining">The training data.</param>
        public ResilientPropagation(BasicNetwork theNetwork, IList<BasicData> theTraining)
        {
            LastError = 1.0;
            _network = theNetwork;
            _training = theTraining;
            _gradients = new GradientCalc(_network, new CrossEntropyErrorFunction(), this);
            _lastDelta = new double[theNetwork.Weights.Length];
            _updateValues = new double[theNetwork.Weights.Length];
            _lastGradients = new double[theNetwork.Weights.Length];

            for (var i = 0; i < _updateValues.Length; i++)
            {
                _updateValues[i] = DEFAULT_INITIAL_UPDATE;
            }
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
            _gradients.Reset();
            _errorCalc.Clear();

            // Calculate gradients for entire training set, RPROP does not do online.
            foreach (var element in _training)
            {
                _gradients.Process(_errorCalc, element.Input, element.Ideal);
            }
            LastError = _errorCalc.Calculate();

            // Apply the gradients according to the RPROP algorithm.
            for (var i = 0; i < _gradients.Gradients.Length; i++)
            {
                var delta = CalculateWeightDelta(_gradients.Gradients, _lastGradients, i);
                _lastGradients[i] = _gradients.Gradients[i];
                _lastDelta[i] = delta;
                _network.Weights[i] += delta;
            }
        }

        /// <summary>
        ///     The error from the last training iteration.
        /// </summary>
        public double LastError { get; private set; }

        /// <summary>
        ///     True, if we are done learning.  Not all learning algorithms know when they are done, in this case
        ///     false is always returned.
        /// </summary>
        public bool Done
        {
            get { return false; }
        }

        /// <summary>
        ///     A string that indicates the status of training.
        /// </summary>
        public string Status
        {
            get { return ""; }
        }

        /// <summary>
        ///     Should be called after the last iteration to make sure training completes any final tasks.
        /// </summary>
        public void FinishTraining()
        {
        }


        /// <summary>
        ///     Calculate the change in weights.
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
            var change = Math.Sign(gradients[index]*lastGradient[index]);
            double weightChange = 0;

            // if the gradient has retained its sign, then we increase the
            // delta so that it will converge faster
            if (change > 0)
            {
                var delta = _updateValues[index]
                            *POSITIVE_ETA;
                delta = Math.Min(delta, DEFAULT_MAX_STEP);
                weightChange = -Math.Sign(gradients[index])*delta;
                _updateValues[index] = delta;
                lastGradient[index] = gradients[index];
            }
            else if (change < 0)
            {
                // if change<0, then the sign has changed, and the last
                // delta was too big
                var delta = _updateValues[index]
                            *NEGATIVE_ETA;
                delta = Math.Max(delta, DELTA_MIN);
                _updateValues[index] = delta;
                weightChange = -_lastDelta[index];
                // set the previous gradent to zero so that there will be no
                // adjustment the next iteration
                lastGradient[index] = 0;
            }
            else if (change == 0)
            {
                // if change==0 then there is no change to the delta
                var delta = _updateValues[index];
                weightChange = -Math.Sign(gradients[index])*delta;
                lastGradient[index] = gradients[index];
            }

            // apply the weight change, if any
            return weightChange;
        }
    }
}