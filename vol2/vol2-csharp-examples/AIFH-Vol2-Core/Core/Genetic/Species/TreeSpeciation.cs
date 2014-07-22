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
