using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;
using AIFH_Vol3_Core.Core.General.Data;
using AIFH_Vol3_Core.Core.Util;
using CsvHelper;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.Timeseries
{
    public class PredictSunspots : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Predict sunspots.";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 13;

        public int INPUT_WINDOW = 10;

        public IList<BasicData> LoadSunspots()
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol3.Resources.sunspots.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read sunspots data from embedded resources.");
                return null;
            }

            // load the data
            var istream = new StreamReader(res);
            CsvReader csv = new CsvReader(istream);
            csv.Read();

            // determine how many entries in file
            int sunspotCount = 0;
            while (csv.Read())
            {
                sunspotCount++;
            }
            Console.WriteLine("Sunspot count:" + sunspotCount);

            // allocate array to hold file
            double[][] dataset = new double[sunspotCount][];

            // read file
            var istream2 = new StreamReader(res);
            CsvReader csv2 = new CsvReader(istream2);

            csv2.Read();

            string[] nextLine;
            int idx = 0;
            while (csv2.Read())
            {
                double ssn = double.Parse(csv2.CurrentRecord[3], CultureInfo.InvariantCulture);
                dataset[idx] = new double[1];
                dataset[idx++][0] = ssn;
            }

            // timseries encode
            IList<BasicData> result = TimeSeriesUtil.SlidingWindow(dataset, this.INPUT_WINDOW, 1, new int[] {0},
                new int[] {0});

            return result;
        }

        public void Process()
        {
            IList<BasicData> trainingData = LoadSunspots();

            BasicNetwork network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, this.INPUT_WINDOW));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 50));
            network.AddLayer(new BasicLayer(new ActivationLinear(), false, 1));
            network.FinalizeStructure();
            network.Reset();

            BackPropagation train = new BackPropagation(network, trainingData, 1e-9, 0.5);
            train.BatchSize = 0;

            PerformIterations(train, 100000, 650, true);
            Query(network, trainingData);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            PredictSunspots prg = new PredictSunspots();
            prg.Process();
        }
    }
}
