package com.heatonresearch.aifh.aco;

/**
 * Defines the cost structure between nodes in a graph.  Costs are assigned at the edges, between nodes.
 */
public interface CostGraph {
    /**
     * Get the cost between two nodes.
     * @param sourceNode The source node.
     * @param targetNode The target node.
     * @return The cost between two nodes.
     */
    double cost(int sourceNode, int targetNode);

    /**
     * @return The size of the graph, in nodes.
     */
    int graphSize();
}
