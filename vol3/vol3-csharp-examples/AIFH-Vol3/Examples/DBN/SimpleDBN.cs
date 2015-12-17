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
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.DBNN;

namespace AIFH_Vol3.Examples.DBN
{
    /// <summary>
    ///     This example trains a deep belief neural network.  The training begins with unsupervised pretraining,
    ///     followed by supervised training of the logisitic regression output layer.
    /// </summary>
    public class SimpleDBN
    {
        public const double LearningRateUnsupervised = 0.1;
        public const double LearningRateSupervised = 0.1;
        public const int K = 1;

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Simple deep belief neural network (DBNN).";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 9;

        // training data
        public static readonly double[][] TRAINING_INPUT =
        {
            new double[] {1, 1, 1, 1, 0, 0, 0, 0},
            new double[] {1, 1, 0, 1, 0, 0, 0, 0},
            new double[] {1, 1, 1, 0, 0, 0, 0, 0},
            new double[] {0, 0, 0, 0, 1, 1, 1, 1},
            new double[] {0, 0, 0, 0, 1, 1, 0, 1},
            new double[] {0, 0, 0, 0, 1, 1, 1, 0}
        };

        public static readonly double[][] TRAINING_IDEAL =
        {
            new double[] {1, 0},
            new double[] {1, 0},
            new double[] {1, 0},
            new double[] {0, 1},
            new double[] {0, 1},
            new double[] {0, 1}
        };

        public static readonly double[][] TEST_INPUT =
        {
            new double[] {0, 1, 1, 1, 0, 0, 0, 0},
            new double[] {1, 0, 1, 1, 0, 0, 0, 0},
            new double[] {0, 0, 0, 0, 0, 1, 1, 1},
            new double[] {0, 0, 0, 0, 1, 0, 1, 1}
        };

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            {
                // Create an dbnn belief network.
                int[] hidden = {2, 3};
                var dbn = new DeepBeliefNetwork(TRAINING_INPUT[0].Length, hidden, TRAINING_IDEAL[0].Length);
                dbn.Random = new MersenneTwisterGenerateRandom(54321);
                dbn.Reset();


                // Layer by layer unsupervised training.
                for (var level = 0; level < hidden.Length; level++)
                {
                    var trainUnsupervised = new UnsupervisedTrainDBN(
                        dbn, level, TRAINING_INPUT, LearningRateUnsupervised, K);
                    for (var i = 0; i < 2000; i++)
                    {
                        trainUnsupervised.Iteration();
                    }
                }

                // Supervised training.
                var trainSupervised = new SupervisedTrainDBN(
                    dbn, TRAINING_INPUT, TRAINING_IDEAL, LearningRateSupervised);
                var iteration = 0;
                do
                {
                    iteration++;
                    trainSupervised.Iteration();
                    Console.WriteLine("Iteration: " + iteration + ", Supervised training: error = "
                                      + trainSupervised.LastError);
                } while (trainSupervised.LastError > 0.001);


                // Use test data.
                foreach (var input in TEST_INPUT)
                {
                    var output = dbn.ComputeRegression(input);
                    Console.WriteLine(string.Join(",", input) + " -> " + string.Join(",", output));
                }
            }
        }
    }
}