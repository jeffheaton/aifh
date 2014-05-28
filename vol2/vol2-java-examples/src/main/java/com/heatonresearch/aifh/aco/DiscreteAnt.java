package com.heatonresearch.aifh.aco;

/**
 * A discrete ant.  It holds a path, as well as nodes visited.
 */
public class DiscreteAnt {

    /**
     * The path.
     */
    private int path[];

    /**
     * The nodes visited.
     */
    private boolean visited[];

    /**
     * The constructor.
     * @param theLength The path length.
     */
    public DiscreteAnt(int theLength) {
        this.path = new int[theLength];
        this.visited = new boolean[theLength];
    }

    /**
     * Visit the specified node, and record.
     * @param currentIndex The current index.
     * @param node The node visited.
     */
    public void visit(int currentIndex, int node) {
        path[currentIndex] = node;
        visited[node] = true;
    }

    /**
     * Was the specified node visited.
     * @param i The node index.
     * @return True, if visited.
     */
    public boolean wasVisited(int i) {
        return visited[i];
    }

    /**
     * Calculate the cost, up to the current point.
     * @param currentIndex The current point.
     * @param graph The cost graph.
     * @return The current cost.
     */
    public double calculateCost(int currentIndex, CostGraph graph) {
        double length = graph.cost(path[currentIndex - 1],path[0]);
        for (int i = 0; i < currentIndex - 1; i++) {
            length += graph.cost(path[i],path[i + 1]);
        }
        return length;
    }

    /**
     * @return The path.
     */
    public int[] getPath() {
        return this.path;
    }

    /**
     * Clear the ant.
     */
    public void clear() {
        for (int i = 0; i < this.visited.length; i++)
            visited[i] = false;
    }
}
