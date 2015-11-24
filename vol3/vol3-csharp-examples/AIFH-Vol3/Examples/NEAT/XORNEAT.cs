using Encog.ML.Data;
using Encog.ML.Data.Basic;
using Encog.ML.EA.Train;
using Encog.Neural.NEAT;
using Encog.Neural.Networks.Training;
using Encog.Util.Simple;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.NEAT
{
    /// <summary>
    /// XOR-NEAT: This example solves the classic XOR operator neural
    /// network problem.However, it uses a NEAT evolving network.
    /// </summary>
    public class XORNEAT
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Use a NEAT neural network for the XOR operator.";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 8;

        /// <summary>
        /// Input for the XOR function.
        /// </summary>
        public static double[][] XORInput = {
                                                new double[2] {0.0, 0.0},
                                                new double[2] {1.0, 0.0},
                                                new double[2] {0.0, 1.0},
                                                new double[2] {1.0, 1.0}
                                            };

        /// <summary>
        /// Ideal output for the XOR function.
        /// </summary>
        public static double[][] XORIdeal = {
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
            NEATPopulation pop = new NEATPopulation(2, 1, 1000);
            pop.Reset();
            pop.InitialConnectionDensity = 1.0; // not required, but speeds processing.
            ICalculateScore score = new TrainingSetScore(trainingSet);
            // train the neural network
            TrainEA train = NEATUtil.ConstructNEATTrainer(pop, score);

            EncogUtility.TrainToError(train, 0.01);

            NEATNetwork network = (NEATNetwork)train.CODEC.Decode(train.BestGenome);

            // test the neural network
            Console.WriteLine(@"Neural Network Results:");
            EncogUtility.Evaluate(network, trainingSet);
        }

        #endregion
    }
}

