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
using System.Text;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Genetic.Trees;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Normalize;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Examples.GP
{
    public class FindEquation
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Learn an equation from a data set using genetic programming (GP).";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 4;


        /// <summary>
        ///     The size of the population.
        /// </summary>
        public const int PopulationSize = 1000;

        /// <summary>
        ///     The maximum number of iterations to allow to have the same score before giving up.
        /// </summary>
        public const int MaxSameSolution = 500;


        /// <summary>
        ///     Generate a random genome.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <param name="eval">The evaluator.</param>
        /// <returns>The random genome.</returns>
        private TreeGenome RandomGenome(IGenerateRandom rnd, EvaluateExpression eval)
        {
            var result = new TreeGenome(eval);
            result.Root = eval.Grow(rnd, 5);
            return result;
        }

        /// <summary>
        ///     Create an initial random population.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="eval">The expression evaluator.</param>
        /// <returns>The new population.</returns>
        private IPopulation InitPopulation(IGenerateRandom rnd, EvaluateExpression eval)
        {
            IPopulation result = new BasicPopulation(PopulationSize, null);

            var defaultSpecies = new BasicSpecies();
            defaultSpecies.Population = result;
            for (int i = 0; i < PopulationSize; i++)
            {
                TreeGenome genome = RandomGenome(rnd, eval);
                defaultSpecies.Add(genome);
            }
            result.GenomeFactory = new TreeGenomeFactory(eval);
            result.Species.Add(defaultSpecies);

            return result;
        }

        /// <summary>
        ///     Process the specified file.
        /// </summary>
        /// <param name="filename">The filename to process.</param>
        public void Process(String filename)
        {
            // read the data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            Stream res = assembly.GetManifestResourceStream("AIFH_Vol2.Resources.simple-poly.csv");

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


            // Extract supervised training.
            IList<BasicData> training = ds.ExtractSupervised(0, 1, 1, 1);


            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();
            var eval = new EvaluateExpression(rnd);
            IPopulation pop = InitPopulation(rnd, eval);
            IScoreFunction score = new ScoreSmallExpression(training, 30);

            IEvolutionaryAlgorithm genetic = new BasicEA(pop, score);
            genetic.AddOperation(0.3, new MutateTree(3));
            genetic.AddOperation(0.7, new CrossoverTree());
            genetic.ShouldIgnoreExceptions = false;


            int sameSolutionCount = 0;
            int iteration = 1;
            double lastSolution = double.MaxValue;
            var builder = new StringBuilder();

            while (sameSolutionCount < MaxSameSolution && iteration < 1000)
            {
                genetic.Iteration();

                double thisSolution = genetic.LastError;

                builder.Length = 0;
                builder.Append("Iteration: ");
                builder.Append(iteration++);
                builder.Append(", Current error = ");
                builder.Append(thisSolution);
                builder.Append(", Best Solution Length = ");
                builder.Append(genetic.BestGenome.Count);

                Console.WriteLine(builder.ToString());

                if (Math.Abs(lastSolution - thisSolution) < 1.0)
                {
                    sameSolutionCount++;
                }
                else
                {
                    sameSolutionCount = 0;
                }

                lastSolution = thisSolution;
            }

            Console.WriteLine("Good solution found:");
            var best = (TreeGenome) genetic.BestGenome;
            Console.WriteLine(eval.DisplayExpressionNormal(best.Root));
            genetic.FinishTraining();
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new FindEquation();
            if (args.Length == 0)
            {
                prg.Process(null);
            }
            else if (args.Length == 1)
            {
                prg.Process(args[0]);
            }
            else
            {
                Console.WriteLine("Specify a filename to fit, or no filename to use a built in simple polynomial.");
            }
        }
    }
}
