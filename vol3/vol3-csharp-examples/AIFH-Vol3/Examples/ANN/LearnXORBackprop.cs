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
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3.Examples.ANN
{
    public class LearnXORBackprop : SimpleLearn
    {
        /// <summary>
        ///     The input necessary for XOR.
        /// </summary>
        public static double[][] XOR_INPUT =
        {
            new[] {0.0, 0.0},
            new[] {1.0, 0.0},
            new[] {0.0, 1.0},
            new[] {1.0, 1.0}
        };

        /// <summary>
        ///     The ideal data necessary for XOR.
        /// </summary>
        public static double[][] XOR_IDEAL =
        {
            new[] {0.0},
            new[] {1.0},
            new[] {1.0},
            new[] {0.0}
        };

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "XOR with Backpropagation.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 6;

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, 2));
            network.AddLayer(new BasicLayer(new ActivationSigmoid(), true, 5));
            network.AddLayer(new BasicLayer(new ActivationSigmoid(), false, 1));
            network.FinalizeStructure();
            network.Reset();

            var trainingData = BasicData.ConvertArrays(XOR_INPUT, XOR_IDEAL);

            // train the neural network
            var train = new BackPropagation(network, trainingData, 0.7, 0.9);

            var epoch = 1;

            do
            {
                train.Iteration();
                Console.WriteLine("Epoch #" + epoch + " Error:" + train.LastError);
                epoch++;
            } while (train.LastError > 0.01);

            // test the neural network
            Console.WriteLine("Neural Network Results:");
            for (var i = 0; i < XOR_INPUT.Length; i++)
            {
                var output = network.ComputeRegression(XOR_INPUT[i]);
                Console.WriteLine(string.Join(",", XOR_INPUT[i])
                                  + ", actual=" + string.Join(",", output)
                                  + ",ideal=" + string.Join(",", XOR_IDEAL[i]));
            }
        }
    }
}