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
using AIFH_Vol2.Core.Genetic.Crossover;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2.Examples.Selection;

namespace AIFH_Vol2.Examples.Operations
{
    public class CrossoverExample
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "An example of the crossover genetic operator.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 2;

        /// <summary>
        ///     Demonstrate the crossover splice operator.  Two offspring will be created by swapping three
        ///     segments of the parents (two cut points). Some genes may repeat.
        /// </summary>
        public static void Splice()
        {
            Console.WriteLine("Crossover Splice");

            // Create a random number generator
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // Create a new population.
            IPopulation pop = new BasicPopulation();
            pop.GenomeFactory = new IntegerArrayGenomeFactory(10);

            // Create a trainer with a very simple score function.  We do not care
            // about the calculation of the score, as they will never be calculated.
            IEvolutionaryAlgorithm train = new BasicEA(pop, new NullScore());

            // Create a splice operator, length = 5.  Use it 1.0 (100%) of the time.
            var opp = new Splice(5);
            train.AddOperation(1.0, opp);

            // Create two parents, the genes are set to 1,2,3,4,5,7,8,9,10
            // and 10,9,8,7,6,5,4,3,2,1.
            var parents = new IntegerArrayGenome[2];
            parents[0] = (IntegerArrayGenome) pop.GenomeFactory.Factor();
            parents[1] = (IntegerArrayGenome) pop.GenomeFactory.Factor();
            for (int i = 1; i <= 10; i++)
            {
                parents[0].Data[i - 1] = i;
                parents[1].Data[i - 1] = 11 - i;
            }

            // Create an array to hold the offspring.
            var offspring = new IntegerArrayGenome[2];

            // Perform the operation
            opp.PerformOperation(rnd, parents, 0, offspring, 0);

            // Display the results
            Console.WriteLine("Parent 1: " + string.Join(",", parents[0].Data));
            Console.WriteLine("Parent 2: " + string.Join(",", parents[1].Data));
            Console.WriteLine("Offspring 1: " + string.Join(",", offspring[0].Data));
            Console.WriteLine("Offspring 2: " + string.Join(",",
                offspring[1].Data));
        }

        /// <summary>
        ///     Demonstrate the crossover splice operator (no repeat).  Two offspring will be created by
        ///     swapping three segments of the parents (two cut points). No repeated genes allowed per offspring.
        /// </summary>
        public static void SpliceNoRepeat()
        {
            Console.WriteLine("Crossover Splice No Repeat");

            // Create a random number generator
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // Create a new population.
            IPopulation pop = new BasicPopulation();
            pop.GenomeFactory = new IntegerArrayGenomeFactory(10);

            // Create a trainer with a very simple score function.  We do not care
            // about the calculation of the score, as they will never be calculated.
            IEvolutionaryAlgorithm train = new BasicEA(pop, new NullScore());

            // Create a splice (no repeat) operator, length = 5.  Use it 1.0 (100%) of the time.
            var opp = new SpliceNoRepeat(5);
            train.AddOperation(1.0, opp);

            // Create two parents, the genes are set to 1,2,3,4,5,7,8,9,10
            // and 10,9,8,7,6,5,4,3,2,1.
            var parents = new IntegerArrayGenome[2];
            parents[0] = (IntegerArrayGenome) pop.GenomeFactory.Factor();
            parents[1] = (IntegerArrayGenome) pop.GenomeFactory.Factor();
            for (int i = 1; i <= 10; i++)
            {
                parents[0].Data[i - 1] = i;
                parents[1].Data[i - 1] = 11 - i;
            }

            // Create an array to hold the offspring.
            var offspring = new IntegerArrayGenome[2];

            // Perform the operation
            opp.PerformOperation(rnd, parents, 0, offspring, 0);

            // Display the results
            Console.WriteLine("Parent 1: " + string.Join(",", parents[0].Data));
            Console.WriteLine("Parent 2: " + string.Join(",", parents[1].Data));
            Console.WriteLine("Offspring 1: " + string.Join(",", offspring[0].Data));
            Console.WriteLine("Offspring 2: " + string.Join(",", offspring[1].Data));
        }


        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            Splice();
            SpliceNoRepeat();
        }
    }
}
