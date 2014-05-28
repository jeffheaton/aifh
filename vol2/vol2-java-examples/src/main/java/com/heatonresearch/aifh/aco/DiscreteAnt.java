/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
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
     *
     * @param theLength The path length.
     */
    public DiscreteAnt(int theLength) {
        this.path = new int[theLength];
        this.visited = new boolean[theLength];
    }

    /**
     * Visit the specified node, and record.
     *
     * @param currentIndex The current index.
     * @param node         The node visited.
     */
    public void visit(int currentIndex, int node) {
        path[currentIndex] = node;
        visited[node] = true;
    }

    /**
     * Was the specified node visited.
     *
     * @param i The node index.
     * @return True, if visited.
     */
    public boolean wasVisited(int i) {
        return visited[i];
    }

    /**
     * Calculate the cost, up to the current point.
     *
     * @param currentIndex The current point.
     * @param graph        The cost graph.
     * @return The current cost.
     */
    public double calculateCost(int currentIndex, CostGraph graph) {
        double length = graph.cost(path[currentIndex - 1], path[0]);
        for (int i = 0; i < currentIndex - 1; i++) {
            length += graph.cost(path[i], path[i + 1]);
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
