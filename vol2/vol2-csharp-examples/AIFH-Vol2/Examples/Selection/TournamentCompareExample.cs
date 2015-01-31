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
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Opp.Selection;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Examples.Selection
{
    /// This example shows how the number of rounds affects the average score of the genome selected by
    /// the tournament selection operator.  A population of 1000 genomes is created with each genome having a
    /// score between 0 and 999.  There is one genome for each score.  Round counts are tried between one and ten.
    /// The average score over 100k selections is reported.  As the number of rounds increases, so does the average
    /// score selected.
    ///
    /// Sample output is shown here:
    ///
    /// Running example: TournamentCompareExample
    /// Rounds: 1, Avg Score: 666
    /// Rounds: 2, Avg Score: 749
    /// Rounds: 3, Avg Score: 798
    /// Rounds: 4, Avg Score: 832
    /// Rounds: 5, Avg Score: 856
    /// Rounds: 6, Avg Score: 874
    /// Rounds: 7, Avg Score: 888
    /// Rounds: 8, Avg Score: 899
    /// Rounds: 9, Avg Score: 908
    /// Rounds: 10, Avg Score: 916
    public class TournamentCompareExample
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Compare tournament selection sizes";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 1;

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            // Create a new population.
            IPopulation pop = new BasicPopulation();
            ISpecies species = pop.CreateSpecies();

            // Create 1000 genomes, assign the score to be the index number.
            for (int i = 0; i < 1000; i++)
            {
                IGenome genome = new IntegerArrayGenome(1);
                genome.Score = i;
                genome.AdjustedScore = i;
                pop.Species[0].Add(genome);
            }

            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // Create a trainer with a very simple score function.  We do not care
            // about the calculation of the score, as they will never be calculated.
            // We only care that we are maximizing.
            IEvolutionaryAlgorithm train = new BasicEA(pop, new NullScore());

            // Perform the test for round counts between 1 and 10.
            for (int roundCount = 1; roundCount <= 10; roundCount++)
            {
                var selection = new TournamentSelection(train, roundCount);
                int sum = 0;
                int count = 0;
                for (int i = 0; i < 100000; i++)
                {
                    int genomeID = selection.PerformSelection(rnd, species);
                    IGenome genome = species.Members[genomeID];
                    sum += (int) genome.AdjustedScore;
                    count++;
                }
                sum /= count;
                Console.WriteLine("Rounds: " + roundCount + ", Avg Score: " + sum);
            }
        }
    }
}
