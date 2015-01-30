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
using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.ACO
{
    /// <summary>
    /// The ant colony optimization (ACO) algorithm finds an optimal path through a graph.  It works by establishing
    /// pheromone trails between the graph nodes.  The pheromone trails increase in strength as ants travel over the
    /// edges of the graph.  The pheromone trails decrease over time.  The discrete version of ACO arranges a path
    /// to visit the nodes of a graph, that minimizes cost.
    ///
    /// References:
    ///
    /// http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms
    ///
    /// M. Dorigo, Optimization, Learning and Natural Algorithms, PhD thesis, Politecnico di Milano, Italy, 1992.
    /// </summary>
    public class DiscreteACO
    {
        /// <summary>
        /// The pheromone trails between graph segments.
        /// </summary>
        private readonly double[][] _pheromone;

        /// <summary>
        /// A graph of costs.
        /// </summary>
        private readonly ICostGraph _graph;

        /// <summary>
        /// The ants.
        /// </summary>
        private readonly IList<DiscreteAnt> _ants = new List<DiscreteAnt>();

        /// <summary>
        /// The initial value of the pheromone trails.
        /// </summary>
        public const double INITIAL_PHEROMONE = 1.0;

        /// <summary>
        /// Constant that defines the attractiveness of the pheromone trail.
        /// </summary>
        public double Alpha { get; set; }

        /// <summary>
        /// Constant that defines the attractiveness of better state transitions (from one node to another).
        /// </summary>
        public double Beta { get; set; }

        /// <summary>
        /// Constant that defines how quickly the pheromone path evaporates.
        /// </summary>
        public double Evaporation { get; set; }

        /// <summary>
        /// The amount of pheromone that the nodes of a path share for a trip.
        /// </summary>
        public double Q { get; set; }

        /// <summary>
        /// The base probability.
        /// </summary>
        public double PR { get; set; }

        /// <summary>
        /// A random number generator.
        /// </summary>
        public IGenerateRandom Random { get; set; }

        /// <summary>
        /// The current best path.
        /// </summary>
        private readonly int[] _bestPath;

        /// <summary>
        /// The cost of the best path.  We are trying to minimize this.
        /// </summary>
        private double _bestCost;

        /// <summary>
        /// The constructor. 
        /// </summary>
        /// <param name="theGraph">The graph that we are seeking a minimal path through.</param>
        /// <param name="theAntCount">The number of ants to use.</param>
        public DiscreteACO(ICostGraph theGraph, int theAntCount)
        {
            Alpha = 1;
            Beta = 5;
            Evaporation = 0.5;
            Q = 500;
            PR = 0.01;
            Random = new MersenneTwisterGenerateRandom();

            int len = theGraph.Count;
            _graph = theGraph;
            _pheromone = new double[len][];
            _bestPath = new int[len];
            _bestCost = double.PositiveInfinity;

            for (int i = 0; i < len; i++)
            {
                _pheromone[i] = new double[len];
                for (int j = 0; j < len; j++)
                {
                    _pheromone[i][j] = INITIAL_PHEROMONE;
                }
            }

            for (int i = 0; i < theAntCount; i++)
            {
                _ants.Add(new DiscreteAnt(len));
            }

        }

        /// <summary>
        /// Calculate the probability of a given ant moving to any of the next nodes. 
        /// </summary>
        /// <param name="currentIndex">The index into the path.</param>
        /// <param name="ant">The ant.</param>
        /// <returns>The probability of moving to the next node.</returns>
        private double[] CalculateProbability(int currentIndex, DiscreteAnt ant)
        {
            double[] result = new double[_graph.Count];
            int i = ant.Path[currentIndex - 1];

            double d = 0.0;
            for (int l = 0; l < _graph.Count; l++)
                if (!ant.WasVisited(l))
                    d += Math.Pow(_pheromone[i][l], Alpha)
                            * Math.Pow(1.0 / _graph.Cost(i, l), Beta);


            for (int j = 0; j < _graph.Count; j++)
            {
                if (ant.WasVisited(j))
                {
                    result[j] = 0.0;
                }
                else
                {
                    double n = Math.Pow(_pheromone[i][j], Alpha)
                            * Math.Pow(1.0 / _graph.Cost(i, j), Beta);
                    result[j] = n / d;
                }
            }
            return result;

        }

        /// <summary>
        /// Choose the next node for an ant to visit.  This is based on probability. 
        /// </summary>
        /// <param name="currentIndex">The step we are at in the path.</param>
        /// <param name="ant">The ant being evaluated.</param>
        /// <returns>The node we will move into.</returns>
        private int PickNextNode(int currentIndex, DiscreteAnt ant)
        {
            if (currentIndex == 0 || Random.NextDouble() < PR)
            {
                int index;
                do
                {
                    index = Random.NextInt(0, _graph.Count);
                } while (ant.WasVisited(index));
                return index;
            }

            double[] prob = CalculateProbability(currentIndex, ant);

            double r = Random.NextDouble();
            double sum = 0;
            for (int i = 0; i < _graph.Count; i++)
            {
                sum += prob[i];
                if (sum >= r)
                    return i;
            }
            // should not happen!
            return -1;
        }

        /// <summary>
        /// Update the pheromone levels both for ants traveling and evaporation.
        /// </summary>
        private void UpdatePheromone()
        {
            // Cause evaporation.
            for (int i = 0; i < _pheromone.Length; i++)
                for (int j = 0; j < _pheromone[i].Length; j++)
                    _pheromone[i][j] *= Evaporation;

            // Adjust for ants.
            foreach (DiscreteAnt a in _ants)
            {
                double d = Q / a.CalculateCost(_graph.Count, _graph);
                for (int i = 0; i < _graph.Count - 1; i++)
                {
                    _pheromone[a.Path[i]][a.Path[i + 1]] += d;
                }
                _pheromone[a.Path[_graph.Count - 1]][a.Path[0]] += d;
            }
        }

        /// <summary>
        /// Move the ants forward on their path.
        /// </summary>
        private void March()
        {
            for (int currentIndex = 0; currentIndex < _graph.Count; currentIndex++)
            {
                foreach (DiscreteAnt a in _ants)
                {
                    int next = PickNextNode(currentIndex, a);
                    a.Visit(currentIndex, next);
                }
            }
        }

        /// <summary>
        /// Reset the ants.
        /// </summary>
        private void SetupAnts()
        {
            foreach (DiscreteAnt a in _ants)
            {
                a.Clear();
            }
        }

        /// <summary>
        /// Update the best path.
        /// </summary>
        private void UpdateBest()
        {
            int[] bestPathFound = null;

            foreach (DiscreteAnt a in _ants)
            {
                double cost = a.CalculateCost(_graph.Count, _graph);
                if (cost < _bestCost)
                {
                    bestPathFound = a.Path;
                    _bestCost = cost;
                }
            }

            if (bestPathFound != null)
            {
                Array.Copy(bestPathFound, _bestPath, _bestPath.Length);
            }
        }


        /// <summary>
        /// Perform one iteration.
        /// </summary>
        public void Iteration()
        {
            SetupAnts();
            March();
            UpdatePheromone();
            UpdateBest();
        }

        /// <summary>
        /// The best tour/path.
        /// </summary>
        public int[] BestTour
        {
            get
            {
                return _bestPath;
            }
        }

        /// <summary>
        /// The best cost.
        /// </summary>
        public double BestCost
        {
            get
            {
                return _bestCost;
            }
        }

        /// <summary>
        /// The pheromone levels.
        /// </summary>
        public double[][] Pheromone
        {
            get
            {
                return _pheromone;
            }
        }

        /// <summary>
        /// The cost graph.
        /// </summary>
        public ICostGraph Graph
        {
            get
            {
                return _graph;
            }
        }

        /// <summary>
        /// The ants.
        /// </summary>
        public IList<DiscreteAnt> Ants
        {
            get
            {
                return _ants;
            }
        }

        /// <summary>
        /// The best path.
        /// </summary>
        public int[] BestPath
        {
            get
            {
                return _bestPath;
            }
        }
    }
}
