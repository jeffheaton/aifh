using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Learning.Score;
using AIFH_Vol3.Core.Normalize;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;
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
    /// This example shows how to create a simple classification neural network for the Iris dataset.
    /// An input layer with 4 neurons is used for the 4 input measurements.A dense (BasicLayer) ReLU layer
    /// is used for the hidden and a softmax on the output.Because this is a classification problem,
    /// a Softmax is used for the output.  This causes the 3 outputs to specify the relative probability
    /// of the iris measurements being one of the 3 output species.
    ///
    /// The input data are normalized to the range [-1,1].
    /// </summary>
    public class LearnIrisBackprop : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Iris ANN Backpropagation.";

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
            var res = assembly.GetManifestResourceStream("AIFH_Vol3.Resources.iris.csv");

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
            IDictionary<String, int> species = ds.EncodeOneOfN(4);

            IList<BasicData> trainingData = ds.ExtractSupervised(0, 4, 4, 3);

            BasicNetwork network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, 4));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 20));
            network.AddLayer(new BasicLayer(new ActivationSoftMax(), false, 3));
            network.FinalizeStructure();
            network.Reset();

            BackPropagation train = new BackPropagation(network, trainingData, 0.001, 0.9);

            PerformIterations(train, 100000, 0.02, true);
            QueryOneOfN(network, trainingData, species);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnIrisBackprop();
            prg.Process();
        }

    }
}
