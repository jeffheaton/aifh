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
using AIFH_Vol2.Core.ACO;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2.Examples.GA.TSP;

namespace AIFH_Vol2.Examples.ACO
{
    /// <summary>
    /// 
    /// </summary>
    public class TSPExampleACO : ICostGraph
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Find a traveling salesman path using ACO.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 7;

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
        ///     The cities to visit.
        /// </summary>
        private City[] _cities;

        public double Cost(int sourceNode, int targetNode)
        {
            City city1 = _cities[sourceNode];
            City city2 = _cities[targetNode];
            return city1.Proximity(city2);
        }

        public int Count
        {
            get { return Cities; }
        }

        /// <summary>
        ///     Place the cities in random locations.
        /// </summary>
        private void InitCities()
        {
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();
            _cities = new City[Cities];
            for (int i = 0; i < _cities.Length; i++)
            {
                int xPos = rnd.NextInt(MapSize);
                int yPos = rnd.NextInt(MapSize);

                _cities[i] = new City(xPos, yPos);
            }
        }

        /// <summary>
        ///     Display the cities in the final path.
        /// </summary>
        /// <param name="path">The solution path.</param>
        public void DisplaySolution(int[] path)
        {
            bool first = true;

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
            var builder = new StringBuilder();

            InitCities();
            var aco = new DiscreteACO(this, 50);

            int sameSolutionCount = 0;
            int iteration = 1;
            double lastSolution = double.MaxValue;

            while (sameSolutionCount < MaxSameSolution)
            {
                aco.Iteration();

                double thisSolution = aco.BestCost;

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
            int[] best = aco.BestTour;
            DisplaySolution(best);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var solve = new TSPExampleACO();
            solve.Solve();
        }
    }
}
