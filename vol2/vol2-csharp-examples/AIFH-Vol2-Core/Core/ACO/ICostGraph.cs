using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.ACO
{
    /// <summary>
    /// Defines the cost structure between nodes in a graph.  Costs are assigned at the edges, between nodes.
    /// </summary>
    public interface ICostGraph
    {
        /// <summary>
        /// Get the cost between two nodes.
        /// </summary>
        /// <param name="sourceNode">The source node.</param>
        /// <param name="targetNode">The target node.</param>
        /// <returns>The cost between two nodes.</returns>
        double Cost(int sourceNode, int targetNode);

        /// <summary>
        /// The size of the graph, in nodes.
        /// </summary>
        int Count { get; }
    }
}
