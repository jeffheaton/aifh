using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.Learning;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    /// Supervised training for the DBN.  Used to train the output layer with labels.
    /// </summary>
    public class SupervisedTrainDBN: ILearningMethod
    {
        /// <summary>
        /// The network to train.
        /// </summary>
        private DeepBeliefNetwork _network;

        /// <summary>
        /// The input (x) for the training.
        /// </summary>
        private double[][] _trainingInput;

        /// <summary>
        /// The expected output (y, or labels).
        /// </summary>
        private double[][] _trainingIdeal;

        /// <summary>
        /// The learning rate.
        /// </summary>
        private double _learningRate;

        /// <summary>
        /// The error calculation to use.
        /// </summary>
        public IErrorCalculation ErrorCalc { get; set; }
        
        /// <summary>
        /// Construct the supervised trainer for DBN. 
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

        /// <inheritdoc/>
    public void Iteration()
        {
            double[] layerInput = new double[0];
            double[] prevLayerInput;

            ErrorCalc.Clear();
            for (int n = 0; n < _trainingInput.Length; n++)
            {

                for (int i = 0; i < _network.Layers.Length; i++)
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

        /// <inheritdoc/>
    public double LastError
        {
            get
            {
                return ErrorCalc.Calculate();
            }
        }

        /// <summary>
        /// Done?
        /// </summary>
    public bool Done
        { get
            {
                return false;
            }
        }

        /// <inheritdoc/>
    public String Status
        {
            get
            {
                return "";
            }
        }

        /// <inheritdoc/>
    public void FinishTraining()
        {

        }
        
        /// <summary>
        /// Train the logistic layer, the output layer. 
        /// </summary>
        /// <param name="input">The input (x).</param>
        /// <param name="ideal">The expected output (y, or labels).</param>
        private void TrainLogisticLayer(double[] input, double[] ideal)
        {
            double[] pYgivenX = new double[_network.LogLayer.OutputCount];
            double[] dy = new double[_network.LogLayer.OutputCount];

            for (int i = 0; i < _network.LogLayer.OutputCount; i++)
            {
                pYgivenX[i] = 0;
                for (int j = 0; j < _network.LogLayer.InputCount; j++)
                {
                    pYgivenX[i] += _network.LogLayer.Weights[i][j] * input[j];
                }
                pYgivenX[i] += _network.LogLayer.Bias[i];
            }
            _network.LogLayer.Softmax(pYgivenX);



            for (int i = 0; i < _network.LogLayer.OutputCount; i++)
            {
                dy[i] = ideal[i] - pYgivenX[i];
                ErrorCalc.UpdateError(ideal[i], pYgivenX[i]);

                for (int j = 0; j < _network.LogLayer.InputCount; j++)
                {
                    _network.LogLayer.Weights[i][j] += _learningRate * dy[i] * input[j] / _trainingInput.Length;
                }

                _network.LogLayer.Bias[i] += _learningRate * dy[i] / _trainingInput.Length;
            }
        }
    }
}
