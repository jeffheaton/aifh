package com.heatonresearch.aifh.aco;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/7/14
 * Time: 4:34 AM
 * To change this template use File | Settings | File Templates.
 */
public class DiscreteAnt {
    private int path[];
    private boolean visited[];

    public DiscreteAnt(int theLength) {
        this.path = new int[theLength];
        this.visited = new boolean[theLength];
    }

    public void visit(int currentIndex, int node) {
        path[currentIndex] = node;
        visited[node] = true;
    }

    public boolean wasVisited(int i) {
        return visited[i];
    }

    public double calculateCost(int currentIndex, CostGraph graph) {
        double length = graph.cost(path[currentIndex - 1],path[0]);
        for (int i = 0; i < currentIndex - 1; i++) {
            length += graph.cost(path[i],path[i + 1]);
        }
        return length;
    }

    public int[] getPath() {
        return this.path;
    }

    public void clear() {
        for (int i = 0; i < this.visited.length; i++)
            visited[i] = false;
    }
}
