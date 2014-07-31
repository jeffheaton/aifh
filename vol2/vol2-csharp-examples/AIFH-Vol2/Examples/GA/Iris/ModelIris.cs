﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Genetic.Crossover;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Genetic.Mutate;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Normalize;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2.Examples.Util;

namespace AIFH_Vol2.Examples.GA.Iris
{
    /// <summary>
    ///     Learn the Iris data set with a RBF network trained by a genetic algorithm.
    /// </summary>
    public class ModelIris : SimpleLearn
    {
        /// <summary>
        ///     The size of the population.
        /// </summary>
        public const int PopulationSize = 1000;


        /// <summary>
        ///     The number of RBF functions to use in the network.
        /// </summary>
        public const int RbfCount = 5;

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Learn the Iris data set with a RBF network trained by a genetic algorithm.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 3;

        /// <summary>
        ///     Create an initial population.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <param name="codec">The codec, the type of network to use.</param>
        /// <returns>The population.</returns>
        public static IPopulation InitPopulation(IGenerateRandom rnd, RBFNetworkGenomeCODEC codec)
        {
            // Create a RBF network to get the length.
            var network = new RBFNetwork(codec.InputCount, codec.RbfCount, codec.OutputCount);
            int size = network.LongTermMemory.Length;

            // Create a new population, use a single species.
            IPopulation result = new BasicPopulation(PopulationSize, new DoubleArrayGenomeFactory(size));
            var defaultSpecies = new BasicSpecies {Population = result};
            result.Species.Add(defaultSpecies);

            // Create a new population of random networks.
            for (int i = 0; i < PopulationSize; i++)
            {
                var genome = new DoubleArrayGenome(size);
                network.Reset(rnd);
                Array.Copy(network.LongTermMemory, 0, genome.Data, 0, size);
                defaultSpecies.Add(genome);
            }

            // Set the genome factory to use the double array genome.
            result.GenomeFactory = new DoubleArrayGenomeFactory(size);

            return result;
        }

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
            istream.Close();

            var codec = new RBFNetworkGenomeCODEC(4, RbfCount, 3);

            IList<BasicData> trainingData = ds.ExtractSupervised(0,
                codec.InputCount, 4, codec.OutputCount);

            IPopulation pop = InitPopulation(rnd, codec);

            IScoreFunction score = new ScoreRegressionData(trainingData);

            var genetic = new BasicEA(pop, score) {CODEC = codec};
            genetic.AddOperation(0.7, new Splice(codec.Size/3));
            genetic.AddOperation(0.3, new MutatePerturb(0.1));


            PerformIterations(genetic, 100000, 0.05, true);

            var winner = (RBFNetwork) codec.Decode(genetic.BestGenome);

            QueryOneOfN(winner, trainingData, species);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var solve = new ModelIris();
            solve.Process();
        }
    }
}