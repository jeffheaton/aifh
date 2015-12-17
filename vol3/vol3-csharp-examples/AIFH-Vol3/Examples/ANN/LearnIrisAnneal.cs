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
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Learning.Score;
using AIFH_Vol3.Core.Normalize;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3.Examples.Learning;

namespace AIFH_Vol3.Examples.ANN
{
    /// <summary>
    /// Use a RBF network to learn the Iris data set, trained by hill climbing.
    /// </summary>
    public class LearnIrisAnneal : SimpleLearn
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Iris ANN annealed.";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 4;

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

            var network = new RBFNetwork(4, 4, 2);
            network.Reset(new MersenneTwisterGenerateRandom());
            IScoreFunction score = new ScoreRegressionData(trainingData);
            var train = new TrainAnneal(network, score);
            PerformIterations(train, 100000, 0.01, true);
            QueryOneOfN(network, trainingData, species);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new LearnIrisAnneal();
            prg.Process();
        }
    }
}
