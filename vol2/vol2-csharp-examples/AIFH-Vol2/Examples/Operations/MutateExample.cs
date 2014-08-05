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
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Genetic.Mutate;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2.Examples.Selection;

namespace AIFH_Vol2.Examples.Operations
{
    /// <summary>
    /// This example shows how two different mutate operators create an offspring from a genome.
    /// 
    /// Sample output:
    /// 
    /// Running example: MutateExample
    /// Mutate shuffle
    /// Parent: 1,2,3,4,5
    /// Offspring: 2,1,3,4,5
    /// Mutate Perturb
    /// Parent: 1,2,3,4,5
    /// Offspring: 1.05091550046888,2.08522051958956,3.08803087473105,3.9682652562863,4.
    /// 94037166787799
    /// </summary>
    public class MutateExample
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "An example of the mutate genetic operator.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 2;

        /// <summary>
        ///     Demonstrate the mutate shuffle operator.  An offspring will be created by swapping two
        ///     individual genes.
        /// </summary>
        public static void MutateShuffle()
        {
            Console.WriteLine("Mutate shuffle");

            // Create a random number generator
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // Create a new population.
            IPopulation pop = new BasicPopulation();
            pop.GenomeFactory = new IntegerArrayGenomeFactory(5);

            // Create a trainer with a very simple score function.  We do not care
            // about the calculation of the score, as they will never be calculated.
            IEvolutionaryAlgorithm train = new BasicEA(pop, new NullScore());

            // Create a shuffle operator.  Use it 1.0 (100%) of the time.
            var opp = new MutateShuffle();
            train.AddOperation(1.0, opp);

            // Create a single parent, the genes are set to 1,2,3,4,5.
            var parents = new IntegerArrayGenome[1];
            parents[0] = (IntegerArrayGenome)pop.GenomeFactory.Factor();
            for (int i = 1; i <= 5; i++)
            {
                parents[0].Data[i - 1] = i;
            }

            // Create an array to hold the offspring.
            var offspring = new IntegerArrayGenome[1];
            offspring[0] = new IntegerArrayGenome(5);

            // Perform the operation
            opp.PerformOperation(rnd, parents, 0, offspring, 0);

            // Display the results
            Console.WriteLine("Parent: " + string.Join(",", parents[0].Data));
            Console.WriteLine("Offspring: " + string.Join(",", offspring[0].Data));
        }

        /// <summary>
        ///  Demonstrate the mutate peterb operator.  An offspring will be created by randomly changing each
        /// gene.
        /// </summary>
        public static void MutatePeterb()
        {
            Console.WriteLine("Mutate Perturb");

            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // Create a new population.
            IPopulation pop = new BasicPopulation();
            pop.GenomeFactory = new DoubleArrayGenomeFactory(5);

            // Create a trainer with a very simple score function.  We do not care
            // about the calculation of the score, as they will never be calculated.
            IEvolutionaryAlgorithm train = new BasicEA(pop, new NullScore());


            var opp = new MutatePerturb(0.1);
            train.AddOperation(1.0, opp);


            // Create a peterb operator.  Use it 1.0 (100%) of the time.
            var parents = new DoubleArrayGenome[1];
            parents[0] = (DoubleArrayGenome)pop.GenomeFactory.Factor();
            parents[0].Population = pop;

            for (int i = 1; i <= 5; i++)
            {
                parents[0].Data[i - 1] = i;
            }

            // Create an array to hold the offspring.
            var offspring = new DoubleArrayGenome[1];
            offspring[0] = new DoubleArrayGenome(5);

            // Perform the operation
            opp.PerformOperation(rnd, parents, 0, offspring, 0);

            // Display the results
            Console.WriteLine("Parent: " + string.Join(",", parents[0].Data));
            Console.WriteLine("Offspring: " + string.Join(",", offspring[0].Data));
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            MutateShuffle();
            MutatePeterb();
        }
    }
}
