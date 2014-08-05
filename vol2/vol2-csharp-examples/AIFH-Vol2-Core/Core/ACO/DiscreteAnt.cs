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
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.ACO
{
    /// <summary>
    /// A discrete ant.  It holds a path, as well as nodes visited.
    /// </summary>
    public class DiscreteAnt
    {
        /// <summary>
        /// The path.
        /// </summary>
        private int[] _path;

        /// <summary>
        /// The nodes visited.
        /// </summary>
        private bool[] _visited;

        /// <summary>
        /// The constructor.
        /// </summary>
        /// <param name="theLength">The path length.</param>
        public DiscreteAnt(int theLength)
        {
            _path = new int[theLength];
            _visited = new bool[theLength];
        }

        /// <summary>
        /// Visit the specified node, and record.
        /// </summary>
        /// <param name="currentIndex">The current index.</param>
        /// <param name="node">The node visited.</param>
        public void Visit(int currentIndex, int node)
        {
            _path[currentIndex] = node;
            _visited[node] = true;
        }

        /// <summary>
        /// Was the specified node visited.
        /// </summary>
        /// <param name="i">The node index.</param>
        /// <returns>True, if visited.</returns>
        public bool WasVisited(int i)
        {
            return _visited[i];
        }

        /// <summary>
        /// Calculate the cost, up to the current point.
        /// </summary>
        /// <param name="currentIndex">The current point.</param>
        /// <param name="graph">The cost graph.</param>
        /// <returns>The current cost.</returns>
        public double CalculateCost(int currentIndex, ICostGraph graph)
        {
            double length = graph.Cost(_path[currentIndex - 1], _path[0]);
            for (int i = 0; i < currentIndex - 1; i++)
            {
                length += graph.Cost(_path[i], _path[i + 1]);
            }
            return length;
        }

        /// <summary>
        /// The path.
        /// </summary>
        public int[] Path
        {
            get
            {
                return _path;
            }
        }

        /// <summary>
        /// Clear the ant.
        /// </summary>
        public void Clear()
        {
            for (int i = 0; i < _visited.Length; i++)
                _visited[i] = false;
        }
    }
}
