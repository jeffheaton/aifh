using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Learning;
using AIFH_Vol1.Core.Learning.Score;
using AIFH_Vol1.Core.Normalize;

namespace AIFH_Vol1.Examples.Learning
{
    /// <summary>
    /// Learn Iris data using RBF network & Greedy Random algorithm
    /// </summary>
    public class LearnIris : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Learn Iris data using RBF network & Greedy Random algorithm";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 7;

        /// <summary>
        /// Run the example.
        /// </summary>
        public void Process()
        {
            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol1.Resources.iris.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read iris data from embedded resources.");
                return;
            }

            // load the data
            var istream = new StreamReader(res);
            DataSet ds = DataSet.Load(istream);
            istream.Close();

            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.NormalizeRange(0, 0, 1);
            ds.NormalizeRange(1, 0, 1);
            ds.NormalizeRange(2, 0, 1);
            ds.NormalizeRange(3, 0, 1);
            IDictionary<String, int> species = ds.EncodeEquilateral(4);

            IList<BasicData> trainingData = ds.ExtractSupervised(0, 4, 4, 2);

            var network = new RBFNetwork(4, 4, 2);
            IScoreFunction score = new ScoreRegressionData(trainingData);
            var train = new TrainGreedyRandom(true, network, score);
            PerformIterations(train, 100000, 0.01, true);
            QueryEquilateral(network, trainingData, species, 0, 1);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnIris();
            prg.Process();
        }
    }
}
