using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.ANN
{
    public class LearnXORRPROP : SimpleLearn
    {
        /// <summary>
        /// The input necessary for XOR.
        /// </summary>
        public static double[][] XOR_INPUT =
        {
            new double[] { 0.0, 0.0 },
            new double[] { 1.0, 0.0 },
            new double[] { 0.0, 1.0 },
            new double[] { 1.0, 1.0 } };

        /// <summary>
        /// The ideal data necessary for XOR.
        /// </summary>
        public static double[][] XOR_IDEAL =
        {
            new double[] { 0.0 },
            new double[] { 1.0 },
            new double[] { 1.0 },
            new double[] { 0.0 } };

        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "XOR with Resilient Propagation (RPROP).";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 7;

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            BasicNetwork network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, 2));
            network.AddLayer(new BasicLayer(new ActivationSigmoid(), true, 5));
            network.AddLayer(new BasicLayer(new ActivationSigmoid(), false, 1));
            network.FinalizeStructure();
            network.Reset();

            IList<BasicData> trainingData = BasicData.ConvertArrays(XOR_INPUT, XOR_IDEAL);

            // train the neural network
            ResilientPropagation train = new ResilientPropagation(network, trainingData);

            int epoch = 1;

            do
            {
                train.Iteration();
                Console.WriteLine("Epoch #" + epoch + " Error:" + train.LastError);
                epoch++;
            } while (train.LastError > 0.01);

            // test the neural network
            Console.WriteLine("Neural Network Results:");
            for (int i = 0; i < XOR_INPUT.Length; i++)
            {
                double[] output = network.ComputeRegression(XOR_INPUT[i]);
                Console.WriteLine(string.Join(",", XOR_INPUT[i])
                        + ", actual=" + string.Join(",", output)
                        + ",ideal=" + string.Join(",", XOR_IDEAL[i]));
            }
        }
    }
}
