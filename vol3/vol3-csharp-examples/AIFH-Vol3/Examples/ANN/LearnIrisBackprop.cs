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
using AIFH_Vol3.Core.Normalize;
using AIFH_Vol3.Examples.Learning;
using AIFH_Vol3_Core.Core.ANN;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3.Examples.ANN
{
    /// <summary>
    ///     This example shows how to create a simple classification neural network for the Iris dataset.
    ///     An input layer with 4 neurons is used for the 4 input measurements.A dense (BasicLayer) ReLU layer
    ///     is used for the hidden and a softmax on the output.Because this is a classification problem,
    ///     a Softmax is used for the output.  This causes the 3 outputs to specify the relative probability
    ///     of the iris measurements being one of the 3 output species.
    ///     The input data are normalized to the range [-1,1].
    /// </summary>
    public class LearnIrisBackprop : SimpleLearn
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Iris ANN Backpropagation.";

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
            var res = assembly.GetManifestResourceStream("AIFH_Vol3.Resources.iris.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read iris data from embedded resources.");
                return;
            }

            // load the data
            var istream = new StreamReader(res);
            var ds = DataSet.Load(istream);
            istream.Close();

            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.NormalizeRange(0, 0, 1);
            ds.NormalizeRange(1, 0, 1);
            ds.NormalizeRange(2, 0, 1);
            ds.NormalizeRange(3, 0, 1);
            var species = ds.EncodeOneOfN(4);

            var trainingData = ds.ExtractSupervised(0, 4, 4, 3);

            var network = new BasicNetwork();
            network.AddLayer(new BasicLayer(null, true, 4));
            network.AddLayer(new BasicLayer(new ActivationReLU(), true, 20));
            network.AddLayer(new BasicLayer(new ActivationSoftMax(), false, 3));
            network.FinalizeStructure();
            network.Reset();

            var train = new BackPropagation(network, trainingData, 0.001, 0.9);

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