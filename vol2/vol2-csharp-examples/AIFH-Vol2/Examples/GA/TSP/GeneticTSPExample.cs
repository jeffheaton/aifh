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
using System.Text;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Genetic.Crossover;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Genetic.Mutate;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Examples.GA.TSP
{
    /// <summary>
    ///     Find the shortest path through several cities with a genetic algorithm (GA).
    ///     This example shows how to use it to find a potential solution to the Traveling Salesman Problem (TSP).
    /// 
    /// Sample output:
    /// 
    /// Running example: GeneticTSPExample
    /// Iteration: 1, Best Path Length = 5109
    /// Iteration: 2, Best Path Length = 5109
    /// Iteration: 3, Best Path Length = 5109
    /// Iteration: 4, Best Path Length = 5109
    /// Iteration: 5, Best Path Length = 5109
    /// ...
    /// Iteration: 98, Best Path Length = 4816
    /// Iteration: 99, Best Path Length = 4816
    /// Iteration: 100, Best Path Length = 4816
    /// Good solution found:
    /// 16>15>13>20>37>17>43>49>12>21>14>3>44>42>32>39>18>30>23>36>48>29>26>1>8>33>10>41
    /// >31>2>7>0>28>11>6>9>40>34>4>22>47>25>24>35>27>46>5>19>45>38
    /// </summary>
    public class GeneticTSPExample
    {
        /// <summary>
        ///     The number of cities to visit.
        /// </summary>
        public const int Cities = 50;

        /// <summary>
        ///     The size of the population.
        /// </summary>
        public const int PopulationSize = 1000;

        /// <summary>
        ///     The square size of the map.
        /// </summary>
        public const int MapSize = 256;

        /// <summary>
        ///     The maximum number of iterations to allow to have the same score before giving up.
        /// </summary>
        public const int MaxSameSolution = 50;

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Use a genetic algorithm (GA) for the travelling salesman problem (TSP).";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 3;

        /// <summary>
        ///     The cities to visit.
        /// </summary>
        private City[] _cities;

        /// <summary>
        ///     The genetic algorithm.
        /// </summary>
        private BasicEA _genetic;

        /// <summary>
        ///     Place the cities in random locations.
        /// </summary>
        /// <param name="rnd">Random number.</param>
        private void InitCities(IGenerateRandom rnd)
        {
            _cities = new City[Cities];
            for (int i = 0; i < _cities.Length; i++)
            {
                int xPos = rnd.NextInt(0, MapSize);
                int yPos = rnd.NextInt(0, MapSize);

                _cities[i] = new City(xPos, yPos);
            }
        }

        /// <summary>
        ///     Generate a random path through cities.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <returns>A genome.</returns>
        private IntegerArrayGenome RandomGenome(IGenerateRandom rnd)
        {
            var result = new IntegerArrayGenome(_cities.Length);
            int[] organism = result.Data;
            var taken = new bool[_cities.Length];

            for (int i = 0; i < organism.Length - 1; i++)
            {
                int icandidate;
                do
                {
                    icandidate = rnd.NextInt(0, organism.Length);
                } while (taken[icandidate]);
                organism[i] = icandidate;
                taken[icandidate] = true;
                if (i == organism.Length - 2)
                {
                    icandidate = 0;
                    while (taken[icandidate])
                    {
                        icandidate++;
                    }
                    organism[i + 1] = icandidate;
                }
            }
            return result;
        }

        /// <summary>
        ///     Create an initial random population of random paths through the cities.
        /// </summary>
        /// <param name="rnd">The random population.</param>
        /// <returns>The population</returns>
        private IPopulation InitPopulation(IGenerateRandom rnd)
        {
            IPopulation result = new BasicPopulation(PopulationSize, null);

            var defaultSpecies = new BasicSpecies();
            defaultSpecies.Population = result;
            for (int i = 0; i < PopulationSize; i++)
            {
                IntegerArrayGenome genome = RandomGenome(rnd);
                defaultSpecies.Add(genome);
            }
            result.GenomeFactory = new IntegerArrayGenomeFactory(_cities.Length);
            result.Species.Add(defaultSpecies);

            return result;
        }


        /// <summary>
        ///     Display the cities in the final path.
        /// </summary>
        /// <param name="solution">The solution to display.</param>
        public void DisplaySolution(IntegerArrayGenome solution)
        {
            bool first = true;
            int[] path = solution.Data;

            foreach (int aPath in path)
            {
                if (!first)
                    Console.Write(">");
                Console.Write("" + aPath);
                first = false;
            }

            Console.WriteLine();
        }

        /// <summary>
        ///     Setup and solve the TSP.
        /// </summary>
        public void Solve()
        {
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();
            var builder = new StringBuilder();

            InitCities(rnd);

            IPopulation pop = InitPopulation(rnd);

            IScoreFunction score = new TSPScore(_cities);

            _genetic = new BasicEA(pop, score);

            _genetic.AddOperation(0.9, new SpliceNoRepeat(Cities / 3));
            _genetic.AddOperation(0.1, new MutateShuffle());

            int sameSolutionCount = 0;
            int iteration = 1;
            double lastSolution = double.MaxValue;

            while (sameSolutionCount < MaxSameSolution)
            {
                _genetic.Iteration();

                double thisSolution = _genetic.LastError;

                builder.Length = 0;
                builder.Append("Iteration: ");
                builder.Append(iteration++);
                builder.Append(", Best Path Length = ");
                builder.Append(thisSolution);

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
            var best = (IntegerArrayGenome)_genetic.BestGenome;
            DisplaySolution(best);
            _genetic.FinishTraining();
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var solve = new GeneticTSPExample();
            solve.Solve();
        }
    }
}
