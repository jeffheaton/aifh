using System;
using System.Collections.Generic;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Learning;
using AIFH_Vol1.Core.Learning.Score;

namespace AIFH_Vol1.Examples.Learning
{
    /// <summary>
    /// Learn a simple polynomial with the Greedy Random algorithm.
    /// </summary>
    public class LearnPolynomial : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Learn a simple polynomial with the Greedy Random algorithm";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 7;

        public IList<BasicData> GenerateTrainingData()
        {
            IList<BasicData> result = new List<BasicData>();

            for (double x = -50; x < 50; x++)
            {
                double y = (2 * Math.Pow(x, 2)) + (4 * x) + 6;
                var pair = new BasicData(1, 1);
                pair.Input[0] = x;
                pair.Ideal[0] = y;
                result.Add(pair);
            }

            return result;
        }


        /// <summary>
        /// Run the example.
        /// </summary>
        public void Process()
        {
            IList<BasicData> trainingData = GenerateTrainingData();
            var poly = new PolynomialFn(3);
            IScoreFunction score = new ScoreRegressionData(trainingData);
            var train = new TrainGreedyRandom(true, poly, score);
            PerformIterations(train, 1000000, 0.01, true);
            Console.WriteLine(poly.ToString());
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnPolynomial();
            prg.Process();
        }
    }
}
