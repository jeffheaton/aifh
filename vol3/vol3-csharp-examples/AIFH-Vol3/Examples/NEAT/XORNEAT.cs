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
using Encog.ML.Data;
using Encog.ML.Data.Basic;
using Encog.Neural.Networks.Training;
using Encog.Neural.NEAT;
using Encog.Util.Simple;

namespace AIFH_Vol3.Examples.NEAT
{
    /// <summary>
    ///     XOR-NEAT: This example solves the classic XOR operator neural
    ///     network problem.However, it uses a NEAT evolving network.
    /// </summary>
    public class XORNEAT
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Use a NEAT neural network for the XOR operator.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 8;

        /// <summary>
        ///     Input for the XOR function.
        /// </summary>
        public static double[][] XORInput =
        {
            new double[2] {0.0, 0.0},
            new double[2] {1.0, 0.0},
            new double[2] {0.0, 1.0},
            new double[2] {1.0, 1.0}
        };

        /// <summary>
        ///     Ideal output for the XOR function.
        /// </summary>
        public static double[][] XORIdeal =
        {
            new double[1] {0.0},
            new double[1] {1.0},
            new double[1] {1.0},
            new double[1] {0.0}
        };

        #region IExample Members

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            IMLDataSet trainingSet = new BasicMLDataSet(XORInput, XORIdeal);
            var pop = new NEATPopulation(2, 1, 1000);
            pop.Reset();
            pop.InitialConnectionDensity = 1.0; // not required, but speeds processing.
            ICalculateScore score = new TrainingSetScore(trainingSet);
            // train the neural network
            var train = NEATUtil.ConstructNEATTrainer(pop, score);

            EncogUtility.TrainToError(train, 0.01);

            var network = (NEATNetwork) train.CODEC.Decode(train.BestGenome);

            // test the neural network
            Console.WriteLine(@"Neural Network Results:");
            EncogUtility.Evaluate(network, trainingSet);
        }

        #endregion
    }
}