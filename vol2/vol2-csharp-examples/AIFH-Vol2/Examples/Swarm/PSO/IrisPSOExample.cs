// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Normalize;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2.Examples.Util;

namespace AIFH_Vol2.Examples.Swarm.PSO
{
    /// <summary>
    /// Learn the Iris data set with a RBF network trained by PSO.
    /// </summary>
    public class IrisPSOExample : SimpleLearn
    {
        /// <summary>
        ///     The number of particles
        /// </summary>
        public const int ParticleCount = 30;

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Fit the iris data set using particle swarm optimization (PSO)";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 6;

        /// <summary>
        ///     Run the example.
        /// </summary>
        public void Process()
        {
            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            Stream res = assembly.GetManifestResourceStream("AIFH_Vol2.Resources.iris.csv");

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

            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.NormalizeRange(0, -1, 1);
            ds.NormalizeRange(1, -1, 1);
            ds.NormalizeRange(2, -1, 1);
            ds.NormalizeRange(3, -1, 1);
            IDictionary<string, int> species = ds.EncodeOneOfN(4);

            var particles = new RBFNetwork[ParticleCount];
            for (int i = 0; i < particles.Length; i++)
            {
                particles[i] = new RBFNetwork(4, 4, 3);
                particles[i].Reset(rnd);
            }

            IList<BasicData> trainingData = ds.ExtractSupervised(0, 4, 4, 3);

            IScoreFunction score = new ScoreRegressionData(trainingData);

            var train = new TrainPSO(particles, score);

            PerformIterations(train, 100000, 0.05, true);

            var winner = (RBFNetwork) train.BestParticle;

            QueryOneOfN(winner, trainingData, species);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new IrisPSOExample();
            prg.Process();
        }
    }
}
