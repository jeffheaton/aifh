using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Learning;
using AIFH_Vol1.Core.Learning.Score;

namespace AIFH_Vol1.Examples.Learning
{
    /// <summary>
    /// Learn the XOR function using RBF network & Greedy Random algorithm
    /// </summary>
    public class LearnXOR : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Learn the XOR function with a RBF Network trained by Greedy Random";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 7;

        /// <summary>
        /// The input necessary for XOR.
        /// </summary>
        public static double[][] XorInput = {
            new [] {0.0, 0.0}, 
            new [] {1.0, 0.0},
            new [] {0.0, 1.0}, 
            new [] {1.0, 1.0}};

        /// <summary>
        /// The ideal data necessary for XOR.
        /// </summary>
        public static double[][] XorIdeal =
        {
            new [] {0.0}, 
            new [] {1.0}, 
            new [] {1.0}, 
            new [] {0.0}
        };

        /// <summary>
        /// Perform the example.
        /// </summary>
        public void Process()
        {
            var trainingData = BasicData.ConvertArrays(XorInput, XorIdeal);
            var network = new RBFNetwork(2, 5, 1);
            var score = new ScoreRegressionData(trainingData);
            var train = new TrainGreedyRandom(true, network, score);
            PerformIterations(train, 1000000, 0.01, true);
            Query(network, trainingData);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnXOR();
            prg.Process();
        }
    }
}
