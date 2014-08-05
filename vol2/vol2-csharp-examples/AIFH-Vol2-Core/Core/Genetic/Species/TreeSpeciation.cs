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
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Genetic.Trees;
using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Species
{
    public class TreeSpeciation : ThresholdSpeciation
    {
        /// <summary>
        /// Compare two nodes.
        /// </summary>
        /// <param name="result">The result of previous comparisons.</param>
        /// <param name="node1">The first node to compare.</param>
        /// <param name="node2">The second node to compare.</param>
        /// <returns>The result.</returns>
        private double CompareNode(double result, TreeGenomeNode node1,
                                   TreeGenomeNode node2)
        {
            double newResult = result;

            int node1Size = node1.Children.Count;
            int node2Size = node2.Children.Count;
            int childNodeCount = Math.Max(node1Size, node2Size);

            for (int i = 0; i < childNodeCount; i++)
            {
                if (i < node1Size && i < node2Size)
                {
                    TreeGenomeNode childNode1 = node1.Children[i];
                    TreeGenomeNode childNode2 = node2.Children[i];
                    newResult = CompareNode(newResult, childNode1, childNode2);
                }
                else
                {
                    newResult++;
                }
            }

            return newResult;
        }

        public override double GetCompatibilityScore(IGenome genome1, IGenome genome2)
        {
            return CompareNode(0, ((TreeGenome)genome1).Root,
                    ((TreeGenome)genome2).Root);
        }
    }
}
