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
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3.Examples.ANN
{
    public class LearnDigitsDropout : SimpleLearn
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "MNIST Digits Dropout Neural Network.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 12;

        public static readonly int MNIST_DEPTH = 1;

        public void Process()
        {
            Console.WriteLine("Please wait, reading MNIST training data.");
            var dir = AppDomain.CurrentDomain.BaseDirectory;
            var trainingReader = LearnDigitsBackprop.LoadMNIST(dir, true, MNIST_DEPTH);
            var validationReader = LearnDigitsBackprop.LoadMNIST(dir, false, MNIST_DEPTH);

            Console.WriteLine("Training set size: " + trainingReader.NumImages);
            Console.WriteLine("Validation set size: " + validationReader.NumImages);

            var inputCount = trainingReader.Data[0].Input.Length;
            var outputCount = trainingReader.Data[0].Ideal.Length;

            var network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, inputCount));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 100));
            network.AddLayer(new DropoutLayer(new ActivationReLU(), true, 50, 0.5));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 25));
            network.AddLayer(new BasicLayer(new ActivationSoftMax(), false, outputCount));
            network.FinalizeStructure();
            network.Reset();

            // train the neural network
            Console.WriteLine("Training neural network.");
            var train = new BackPropagation(network, trainingReader.Data, 1e-4, 0.9);
            train.L1 = 0;
            train.L2 = 1e-11;

            PerformIterationsClassifyEarlyStop(train, network, validationReader.Data, 5);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnDigitsDropout();
            prg.Process();
        }
    }
}