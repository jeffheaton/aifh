using System;
using System.IO;
using System.Reflection;
using AIFH_Vol1.Core.General.Fns.Link;
using AIFH_Vol1.Core.Normalize;
using AIFH_Vol1.Core.Regression;
using AIFH_Vol1.Examples.Learning;

namespace AIFH_Vol1.Examples.Regression
{
    /// <summary>
    /// Example that uses a GLM to predict the probability of breast cancer.
    /// </summary>
    public class GLMExample : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Use a GLM to predict the probability of breast cancer";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 10;

        /// <summary>
        /// Run the example.
        /// </summary>
        public void Process()
        {
            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol1.Resources.breast-cancer-wisconsin.csv");

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

            ds.DeleteUnknowns();
            ds.DeleteColumn(0);
            ds.ReplaceColumn(9, 4, 1, 0);
            var trainingData = ds.ExtractSupervised(0, 9, 9, 1);

            var reg = new MultipleLinearRegression(9) {LinkFunction = new LogitLinkFunction()};
            var train = new TrainReweightLeastSquares(reg, trainingData);

            int iteration = 0;
            do
            {
                iteration++;
                train.Iteration();
                Console.WriteLine("Iteration #" + iteration + ", Error: " + train.Error);
            } while (iteration < 1000 && train.Error > 0.01);

            Query(reg, trainingData);
            Console.WriteLine("Error: " + train.Error);


        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new GLMExample();
            prg.Process();
        }
    }
}
