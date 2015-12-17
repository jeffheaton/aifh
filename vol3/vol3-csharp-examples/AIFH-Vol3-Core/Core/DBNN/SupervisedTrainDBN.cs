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
using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.Learning;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    ///     Supervised training for the DBN.  Used to train the output layer with labels.
    /// </summary>
    public class SupervisedTrainDBN : ILearningMethod
    {
        /// <summary>
        ///     The learning rate.
        /// </summary>
        private readonly double _learningRate;

        /// <summary>
        ///     The network to train.
        /// </summary>
        private readonly DeepBeliefNetwork _network;

        /// <summary>
        ///     The expected output (y, or labels).
        /// </summary>
        private readonly double[][] _trainingIdeal;

        /// <summary>
        ///     The input (x) for the training.
        /// </summary>
        private readonly double[][] _trainingInput;

        /// <summary>
        ///     Construct the supervised trainer for DBN.
        /// </summary>
        /// <param name="theNetwork">The network to train.</param>
        /// <param name="theTrainingInput">The input (x) to train.</param>
        /// <param name="theTrainingIdeal">The expected output (y, or labels) to train.</param>
        /// <param name="theLearningRate">The learning rate.</param>
        public SupervisedTrainDBN(DeepBeliefNetwork theNetwork, double[][] theTrainingInput, double[][] theTrainingIdeal,
            double theLearningRate)
        {
            _network = theNetwork;
            _trainingInput = theTrainingInput;
            _learningRate = theLearningRate;
            _trainingIdeal = theTrainingIdeal;
            ErrorCalc = new ErrorCalculationMSE();
        }

        /// <summary>
        ///     The error calculation to use.
        /// </summary>
        public IErrorCalculation ErrorCalc { get; set; }

        /// <inheritdoc />
        public void Iteration()
        {
            var layerInput = new double[0];
            double[] prevLayerInput;

            ErrorCalc.Clear();
            for (var n = 0; n < _trainingInput.Length; n++)
            {
                for (var i = 0; i < _network.Layers.Length; i++)
                {
                    if (i == 0)
                    {
                        prevLayerInput = new double[_network.InputCount];
                        Array.Copy(_trainingInput[n], 0, prevLayerInput, 0, _network.InputCount);
                    }
                    else
                    {
                        prevLayerInput = new double[_network.Layers[i].InputCount];
                        Array.Copy(layerInput, 0, prevLayerInput, 0, _network.Layers[i].InputCount);
                    }

                    layerInput = new double[_network.Layers[i].OutputCount];
                    _network.Layers[i].SampleHgivenV(prevLayerInput, layerInput);
                }

                TrainLogisticLayer(layerInput, _trainingIdeal[n]);
            }
        }

        /// <inheritdoc />
        public double LastError
        {
            get { return ErrorCalc.Calculate(); }
        }

        /// <summary>
        ///     Done?
        /// </summary>
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

        /// <summary>
        ///     Train the logistic layer, the output layer.
        /// </summary>
        /// <param name="input">The input (x).</param>
        /// <param name="ideal">The expected output (y, or labels).</param>
        private void TrainLogisticLayer(double[] input, double[] ideal)
        {
            var pYgivenX = new double[_network.LogLayer.OutputCount];
            var dy = new double[_network.LogLayer.OutputCount];

            for (var i = 0; i < _network.LogLayer.OutputCount; i++)
            {
                pYgivenX[i] = 0;
                for (var j = 0; j < _network.LogLayer.InputCount; j++)
                {
                    pYgivenX[i] += _network.LogLayer.Weights[i][j]*input[j];
                }
                pYgivenX[i] += _network.LogLayer.Bias[i];
            }
            _network.LogLayer.Softmax(pYgivenX);


            for (var i = 0; i < _network.LogLayer.OutputCount; i++)
            {
                dy[i] = ideal[i] - pYgivenX[i];
                ErrorCalc.UpdateError(ideal[i], pYgivenX[i]);

                for (var j = 0; j < _network.LogLayer.InputCount; j++)
                {
                    _network.LogLayer.Weights[i][j] += _learningRate*dy[i]*input[j]/_trainingInput.Length;
                }

                _network.LogLayer.Bias[i] += _learningRate*dy[i]/_trainingInput.Length;
            }
        }
    }
}