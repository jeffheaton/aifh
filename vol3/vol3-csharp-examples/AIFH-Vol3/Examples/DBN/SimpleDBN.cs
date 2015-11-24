using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.DBNN;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.DBN
{
    /// <summary>
    /// This example trains a deep belief neural network.  The training begins with unsupervised pretraining,
    /// followed by supervised training of the logisitic regression output layer.
    /// </summary>
    public class SimpleDBN
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Simple deep belief neural network (DBNN).";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 9;

        public const double LearningRateUnsupervised = 0.1;
        public const double LearningRateSupervised = 0.1;
        public const int K = 1;

        // training data
        public static readonly double[][] TRAINING_INPUT = {
            new double[] {1, 1, 1, 1, 0, 0, 0, 0},
            new double[] {1, 1, 0, 1, 0, 0, 0, 0},
            new double[] {1, 1, 1, 0, 0, 0, 0, 0},
            new double[] {0, 0, 0, 0, 1, 1, 1, 1},
            new double[] {0, 0, 0, 0, 1, 1, 0, 1},
            new double[] {0, 0, 0, 0, 1, 1, 1, 0}};

        public static readonly double[][] TRAINING_IDEAL = {
            new double[] {1, 0},
            new double[] {1, 0},
            new double[] {1, 0},
            new double[] {0, 1},
            new double[] {0, 1},
            new double[] {0, 1}};

        public static readonly double[][] TEST_INPUT = {
            new double[] {0, 1, 1, 1, 0, 0, 0, 0},
            new double[] {1, 0, 1, 1, 0, 0, 0, 0},
            new double[] {0, 0, 0, 0, 0, 1, 1, 1},
            new double[] {0, 0, 0, 0, 1, 0, 1, 1}};

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            {

                // Create an dbnn belief network.
                int[] hidden = { 2, 3 };
                DeepBeliefNetwork dbn = new DeepBeliefNetwork(TRAINING_INPUT[0].Length, hidden, TRAINING_IDEAL[0].Length);
                dbn.Random = new MersenneTwisterGenerateRandom(54321);
                dbn.Reset();


                // Layer by layer unsupervised training.
                for (int level = 0; level < hidden.Length; level++)
                {
                    UnsupervisedTrainDBN trainUnsupervised = new UnsupervisedTrainDBN(
                            dbn, level, TRAINING_INPUT, LearningRateUnsupervised, K);
                    for (int i = 0; i < 2000; i++)
                    {
                        trainUnsupervised.Iteration();
                    }
                }

                // Supervised training.
                SupervisedTrainDBN trainSupervised = new SupervisedTrainDBN(
                        dbn, TRAINING_INPUT, TRAINING_IDEAL, LearningRateSupervised);
                int iteration = 0;
                do
                {
                    iteration++;
                    trainSupervised.Iteration();
                    Console.WriteLine("Iteration: " + iteration + ", Supervised training: error = "
                            + trainSupervised.LastError);
                } while (trainSupervised.LastError > 0.001);


                // Use test data.
                foreach (double[] input in TEST_INPUT)
                {
                    double[] output = dbn.ComputeRegression(input);
                    Console.WriteLine(string.Join(",",input) + " -> " + string.Join(",",output));
                }
            }
        }
    }
}
