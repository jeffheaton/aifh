using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Normalize;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;
using AIFH_Vol3_Core.Core.General.Data;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.ANN
{
    /// <summary>
    /// Predict car's MPG using regression.
    /// </summary>
    public class LearnAutoMPGBackprop: SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Auto MPG Backpropagation.";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 8;

        /// <summary>
        /// Run the example.
        /// </summary>
        public void Process()
        {
            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol3.Resources.auto-mpg.data.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read auto MPG data from embedded resources.");
                return;
            }

            // load the data
            var istream = new StreamReader(res);
            DataSet ds = DataSet.Load(istream);
            istream.Close();

            // The following ranges are setup for the Auto MPG data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.

            // First remove some columns that we will not use:
            ds.DeleteColumn(8); // Car name
            ds.DeleteColumn(7); // Car origin
            ds.DeleteColumn(6); // Year
            ds.DeleteUnknowns();

            ds.NormalizeZScore(1);
            ds.NormalizeZScore(2);
            ds.NormalizeZScore(3);
            ds.NormalizeZScore(4);
            ds.NormalizeZScore(5);

            IList<BasicData> trainingData = ds.ExtractSupervised(1, 4, 0, 1);

            IList<IList<BasicData>> splitList = DataUtil.Split(trainingData, 0.75);
            trainingData = splitList[0];
            IList<BasicData> validationData = splitList[1];

            Console.WriteLine("Size of dataset: " + ds.Count);
            Console.WriteLine("Size of training set: " + trainingData.Count);
            Console.WriteLine("Size of validation set: " + validationData.Count);

            int inputCount = trainingData[0].Input.Length;

            BasicNetwork network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, inputCount));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 50));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 25));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 5));
            network.AddLayer(new BasicLayer(new ActivationLinear(), false, 1));
            network.FinalizeStructure();
            network.Reset();

            BackPropagation train = new BackPropagation(network, trainingData, 0.000001, 0.9);

            PerformIterationsEarlyStop(train, network, validationData, 20, new ErrorCalculationMSE());
            Query(network, validationData);

        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnAutoMPGBackprop();
            prg.Process();
        }
    }
}
