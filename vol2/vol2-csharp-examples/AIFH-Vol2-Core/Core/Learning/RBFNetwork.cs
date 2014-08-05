// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
using System.Text;
using AIFH_Vol2.Core.General;
using AIFH_Vol2.Core.General.Fns;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Core.Learning
{
    /// <summary>
    ///     A RBF network is an advanced machine learning algorithm that uses a series of RBF functions to perform
    ///     regression.  It can also perform classification by means of one-of-n encoding.
    ///     The long term memory of a RBF network is made up of the widths and centers of the RBF functions, as well as
    ///     input and output weighting.
    ///     http://en.wikipedia.org/wiki/RBF_network
    /// </summary>
    public class RBFNetwork : IRegressionAlgorithm, IClassificationAlgorithm
    {
        /// <summary>
        /// Index to the input weights.
        /// </summary>
        private readonly int _indexInputWeights;

        /// <summary>
        /// An index to the output weights in the long term memory.
        /// </summary>
        private readonly int _indexOutputWeights;

        /// <summary>
        /// The number of inputs.
        /// </summary>
        private readonly int _inputCount;

        /// <summary>
        /// The weights & RBF parameters.  See constructor for layout.
        /// </summary>
        private readonly double[] _longTermMemory;

        /// <summary>
        /// The output count.
        /// </summary>
        private readonly int _outputCount;

        /// <summary>
        /// The RBF functions.
        /// </summary>
        private readonly IFnRBF[] _rbf;

        /// <summary>
        /// Construct the RBF network. 
        /// </summary>
        /// <param name="theInputCount">The input count.</param>
        /// <param name="rbfCount">The number of RBF functions.</param>
        /// <param name="theOutputCount">The output count.</param>
        public RBFNetwork(int theInputCount, int rbfCount, int theOutputCount)
        {
            _inputCount = theInputCount;
            _outputCount = theOutputCount;

            // calculate input and output weight counts
            // add 1 to output to account for an extra bias node
            int inputWeightCount = _inputCount*rbfCount;
            int outputWeightCount = (rbfCount + 1)*_outputCount;
            int rbfParams = (_inputCount + 1)*rbfCount;
            _longTermMemory = new double[
                inputWeightCount + outputWeightCount + rbfParams];

            _indexInputWeights = 0;
            _indexOutputWeights = inputWeightCount + rbfParams;

            _rbf = new IFnRBF[rbfCount];

            for (int i = 0; i < rbfCount; i++)
            {
                int rbfIndex = inputWeightCount + ((_inputCount + 1)*i);
                _rbf[i] = new GaussianFunction(_inputCount, _longTermMemory, rbfIndex);
            }
        }

        /// <inheritdoc/>
        public double[] LongTermMemory
        {
            get { return _longTermMemory; }
        }

        /// <inheritdoc/>
        public double[] ComputeRegression(double[] input)
        {
            // first, compute the output values of each of the RBFs
            // Add in one additional RBF output for bias (always set to one).
            var rbfOutput = new double[_rbf.Length + 1];
            rbfOutput[rbfOutput.Length - 1] = 1; // bias

            for (int rbfIndex = 0; rbfIndex < _rbf.Length; rbfIndex++)
            {
                // weight the input
                var weightedInput = new double[input.Length];

                for (int inputIndex = 0; inputIndex < input.Length; inputIndex++)
                {
                    int memoryIndex = _indexInputWeights + (rbfIndex*_inputCount) + inputIndex;
                    weightedInput[inputIndex] = input[inputIndex]*_longTermMemory[memoryIndex];
                }

                // calculate the rbf
                rbfOutput[rbfIndex] = _rbf[rbfIndex].Evaluate(weightedInput);
            }

            // second, calculate the output, which is the result of the weighted result of the RBF's.
            var result = new double[_outputCount];

            for (int outputIndex = 0; outputIndex < result.Length; outputIndex++)
            {
                double sum = 0;
                for (int rbfIndex = 0; rbfIndex < rbfOutput.Length; rbfIndex++)
                {
                    // add 1 to rbf length for bias
                    int memoryIndex = _indexOutputWeights + (outputIndex*(_rbf.Length + 1)) + rbfIndex;
                    sum += rbfOutput[rbfIndex]*_longTermMemory[memoryIndex];
                }
                result[outputIndex] = sum;
            }

            // finally, return the result.
            return result;
        }

        
        /// <summary>
        /// Randomize the long term memory, with the specified random number generator.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        public void Reset(IGenerateRandom rnd)
        {
            for (int i = 0; i < _longTermMemory.Length; i++)
            {
                _longTermMemory[i] = rnd.NextDouble(-1, 1);
            }
        }

        /// <inheritdoc />
        public int ComputeClassification(double[] input)
        {
            double[] output = ComputeRegression(input);
            return VectorUtil.MaxIndex(output);
        }

        /// <inheritdoc />
        public override string ToString()
        {
            var result = new StringBuilder();
            result.Append("[RBFNetwork:inputCount=");
            result.Append(_inputCount);
            result.Append(",outputCount=");
            result.Append(_outputCount);
            result.Append(",RBFs=");
            foreach (IFnRBF r in _rbf)
            {
                result.Append(r);
                result.Append(",");
            }
            result.Append("]");
            return result.ToString();
        }
    }
}
