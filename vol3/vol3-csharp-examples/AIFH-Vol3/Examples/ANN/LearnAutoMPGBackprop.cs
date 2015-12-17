// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//

using System;
using System.IO;
using System.Reflection;
using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.Normalize;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;
using AIFH_Vol3_Core.Core.General.Data;

namespace AIFH_Vol3.Examples.ANN
{
    /// <summary>
    ///     Predict car's MPG using regression.
    /// </summary>
    public class LearnAutoMPGBackprop : SimpleLearn
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Auto MPG Backpropagation.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 8;

        /// <summary>
        ///     Run the example.
        /// </summary>
        public void Process()
        {
            // read the iris data from the resources
            var assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol3.Resources.auto-mpg.data.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read auto MPG data from embedded resources.");
                return;
            }

            // load the data
            var istream = new StreamReader(res);
            var ds = DataSet.Load(istream);
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

            var trainingData = ds.ExtractSupervised(1, 4, 0, 1);

            var splitList = DataUtil.Split(trainingData, 0.75);
            trainingData = splitList[0];
            var validationData = splitList[1];

            Console.WriteLine("Size of dataset: " + ds.Count);
            Console.WriteLine("Size of training set: " + trainingData.Count);
            Console.WriteLine("Size of validation set: " + validationData.Count);

            var inputCount = trainingData[0].Input.Length;

            var network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, inputCount));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 50));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 25));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 5));
            network.AddLayer(new BasicLayer(new ActivationLinear(), false, 1));
            network.FinalizeStructure();
            network.Reset();

            var train = new BackPropagation(network, trainingData, 0.000001, 0.9);

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