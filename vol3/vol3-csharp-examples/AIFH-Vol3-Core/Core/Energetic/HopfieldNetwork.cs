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

namespace AIFH_Vol3_Core.Core.Energetic
{
    /// <summary>
    ///     Implements a Hopfield network.
    /// </summary>
    public class HopfieldNetwork : EnergeticNetwork
    {
        /// <summary>
        ///     Default constructor.
        /// </summary>
        public HopfieldNetwork()
        {
        }

        /// <summary>
        ///     Construct a Hopfield with the specified neuron count.
        /// </summary>
        /// <param name="neuronCount">The neuron count.</param>
        public HopfieldNetwork(int neuronCount) : base(neuronCount)
        {
        }

        /// <inheritdoc />
        public int InputCount
        {
            get { return NeuronCount; }
        }

        /// <inheritdoc />
        public int OutputCount
        {
            get { return NeuronCount; }
        }

        /// <summary>
        ///     Note: for Hopfield networks, you will usually want to call the "run"
        ///     method to compute the output.
        ///     This method can be used to copy the input data to the current state.A
        ///     single iteration is then run, and the new current state is returned.
        /// </summary>
        /// <param name="input">The input pattern.</param>
        /// <returns>The new current state.</returns>
        public double[] Compute(double[] input)
        {
            var result = new double[input.Length];
            Array.Copy(input, CurrentState, input.Length);
            Run();

            for (var i = 0; i < CurrentState.Length; i++)
            {
                result[i] = ActivationFunction(CurrentState[i]);
            }
            Array.Copy(CurrentState, 0, result, 0, result.Length);
            return result;
        }

        public double ActivationFunction(double d)
        {
            return d > 0 ? 1 : 0;
        }

        /// <summary>
        ///     Perform one Hopfield iteration.
        /// </summary>
        public void Run()
        {
            for (var toNeuron = 0; toNeuron < NeuronCount; toNeuron++)
            {
                double sum = 0;
                for (var fromNeuron = 0; fromNeuron < NeuronCount; fromNeuron++)
                {
                    sum += CurrentState[fromNeuron]
                           *GetWeight(fromNeuron, toNeuron);
                }
                CurrentState[toNeuron] = ActivationFunction(sum);
            }
        }


        /// <summary>
        ///     Run the network until it becomes stable and does not change from more
        ///     runs.
        /// </summary>
        /// <param name="max">The maximum number of cycles to run before giving up.</param>
        /// <returns>The number of cycles that were run.</returns>
        public int RunUntilStable(int max)
        {
            var done = false;
            var lastStateStr = CurrentState.ToString();
            var currentStateStr = lastStateStr;

            var cycle = 0;
            do
            {
                Run();
                cycle++;

                lastStateStr = CurrentState.ToString();

                if (!currentStateStr.Equals(lastStateStr))
                {
                    if (cycle > max)
                    {
                        done = true;
                    }
                }
                else
                {
                    done = true;
                }

                currentStateStr = lastStateStr;
            } while (!done);

            return cycle;
        }

        public double Energy()
        {
            double t = 0;

            // Calculate first term
            double a = 0;
            for (var i = 0; i < InputCount; i++)
            {
                for (var j = 0; j < OutputCount; j++)
                {
                    a += GetWeight(i, j)*CurrentState[i]*CurrentState[j];
                }
            }
            a *= -0.5;

            // Calculate second term
            double b = 0;
            for (var i = 0; i < InputCount; i++)
            {
                b += CurrentState[i]*t;
            }

            return a + b;
        }
    }
}