using System;
using System.IO;
using System.Reflection;
using AIFH_Vol1.Core.Normalize;
using AIFH_Vol1.Core.Regression;
using AIFH_Vol1.Examples.Learning;

namespace AIFH_Vol1.Examples.Regression
{
    /// <summary>
    /// Linear regression example.
    /// </summary>
    public class LinearRegressionExample : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Linear regression on the abalone data set";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 10;

        public void Process()
        {
            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol1.Resources.abalone.csv");

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

            // The following ranges are setup for the Abalone data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.EncodeOneOfN(0, 0, 1);
            istream.Close();

            var trainingData = ds.ExtractSupervised(0, 10, 10, 1);

            var reg = new MultipleLinearRegression(10);
            var train = new TrainLeastSquares(reg, trainingData);
            train.Iteration();

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
            var prg = new LinearRegressionExample();
            prg.Process();
        }
    }
}
