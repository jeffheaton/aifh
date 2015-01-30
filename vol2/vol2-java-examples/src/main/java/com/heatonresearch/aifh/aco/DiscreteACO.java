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

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.ArrayList;
import java.util.List;

/**
 * The ant colony optimization (ACO) algorithm finds an optimal path through a graph.  It works by establishing
 * pheromone trails between the graph nodes.  The pheromone trails increase in strength as ants travel over the
 * edges of the graph.  The pheromone trails decrease over time.  The discrete version of ACO arranges a path
 * to visit the nodes of a graph, that minimizes cost.
 * <p/>
 * References:
 * <p/>
 * http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms
 * <p/>
 * M. Dorigo, Optimization, Learning and Natural Algorithms, PhD thesis, Politecnico di Milano, Italy, 1992.
 */
public class DiscreteACO {
    /**
     * The pheromone trails between graph segments.
     */
    private final double[][] pheromone;

    /**
     * A graph of costs.
     */
    private final CostGraph graph;

    /**
     * The ants.
     */
    private final List<DiscreteAnt> ants = new ArrayList<DiscreteAnt>();

    /**
     * The initial value of the pheromone trails.
     */
    public static double INITIAL_PHEROMONE = 1.0;

    /**
     * Constant that defines the attractiveness of the pheromone trail.
     */
    private double alpha = 1;

    /**
     * Constant that defines the attractiveness of better state transitions (from one node to another).
     */
    private double beta = 5;

    /**
     * Constant that defines how quickly the pheromone path evaporates.
     */
    private double evaporation = 0.5;

    /**
     * The amount of pheromone that the nodes of a path share for a trip.
     */
    private double q = 500;

    /**
     * The base probability.
     */
    private double pr = 0.01;

    /**
     * A random number generator.
     */
    private GenerateRandom random = new MersenneTwisterGenerateRandom();

    /**
     * The current best path.
     */
    private final int[] bestPath;

    /**
     * The cost of the best path.  We are trying to minimize this.
     */
    private double bestCost;

    /**
     * The constructor.
     *
     * @param theGraph    The graph that we are seeking a minimal path through.
     * @param theAntCount The number of ants to use.
     */
    public DiscreteACO(CostGraph theGraph, int theAntCount) {
        int len = theGraph.graphSize();
        this.graph = theGraph;
        this.pheromone = new double[len][len];
        this.bestPath = new int[len];
        this.bestCost = Double.POSITIVE_INFINITY;

        for (int i = 0; i < len; i++) {
            for (int j = 0; j < len; j++) {
                this.pheromone[i][j] = INITIAL_PHEROMONE;
            }
        }

        for (int i = 0; i < theAntCount; i++) {
            ants.add(new DiscreteAnt(len));
        }

    }

    /**
     * Calculate the probability of a given ant moving to any of the next nodes.
     *
     * @param currentIndex The index into the path.
     * @param ant          The ant.
     * @return The probability of moving to the next node.
     */
    private double[] calculateProbability(int currentIndex, DiscreteAnt ant) {
        double[] result = new double[this.graph.graphSize()];
        int i = ant.getPath()[currentIndex - 1];

        double d = 0.0;
        for (int l = 0; l < this.graph.graphSize(); l++)
            if (!ant.wasVisited(l))
                d += Math.pow(this.pheromone[i][l], alpha)
                        * Math.pow(1.0 / this.graph.cost(i, l), beta);


        for (int j = 0; j < this.graph.graphSize(); j++) {
            if (ant.wasVisited(j)) {
                result[j] = 0.0;
            } else {
                double n = Math.pow(this.pheromone[i][j], alpha)
                        * Math.pow(1.0 / this.graph.cost(i, j), beta);
                result[j] = n / d;
            }
        }
        return result;

    }

    /**
     * Choose the next node for an ant to visit.  This is based on probability.
     *
     * @param currentIndex The step we are at in the path.
     * @param ant          The ant being evaluated.
     * @return The node we will move into.
     */
    private int pickNextNode(int currentIndex, DiscreteAnt ant) {
        if (currentIndex == 0 || this.random.nextDouble() < pr) {
            int index;
            do {
                index = this.random.nextInt(0, this.graph.graphSize());
            } while (ant.wasVisited(index));
            return index;
        }

        double[] prob = calculateProbability(currentIndex, ant);

        double r = this.random.nextDouble();
        double sum = 0;
        for (int i = 0; i < this.graph.graphSize(); i++) {
            sum += prob[i];
            if (sum >= r)
                return i;
        }
        // should not happen!
        return -1;
    }

    /**
     * Update the pheromone levels both for ants traveling and evaporation.
     */
    private void updatePheromone() {
        // Cause evaporation.
        for (int i = 0; i < this.pheromone.length; i++)
            for (int j = 0; j < this.pheromone[i].length; j++)
                this.pheromone[i][j] *= evaporation;

        // Adjust for ants.
        for (DiscreteAnt a : ants) {
            double d = q / a.calculateCost(this.graph.graphSize(), this.graph);
            for (int i = 0; i < this.graph.graphSize() - 1; i++) {
                this.pheromone[a.getPath()[i]][a.getPath()[i + 1]] += d;
            }
            this.pheromone[a.getPath()[this.graph.graphSize() - 1]][a.getPath()[0]] += d;
        }
    }

    /**
     * Move the ants forward on their path.
     */
    private void march() {
        for (int currentIndex = 0; currentIndex < this.graph.graphSize(); currentIndex++) {
            for (DiscreteAnt a : this.ants) {
                int next = pickNextNode(currentIndex, a);
                a.visit(currentIndex, next);
            }
        }
    }

    /**
     * Reset the ants.
     */
    private void setupAnts() {
        for (DiscreteAnt a : this.ants) {
            a.clear();
        }
    }

    /**
     * Update the best path.
     */
    private void updateBest() {
        int[] bestPathFound = null;

        for (DiscreteAnt a : this.ants) {
            double cost = a.calculateCost(this.graph.graphSize(), this.graph);
            if (cost < this.bestCost) {
                bestPathFound = a.getPath();
                this.bestCost = cost;
            }
        }

        if (bestPathFound != null) {
            System.arraycopy(bestPathFound, 0, this.bestPath, 0, this.bestPath.length);
        }
    }


    /**
     * Perform one iteration.
     */
    public void iteration() {
        setupAnts();
        march();
        updatePheromone();
        updateBest();
    }

    /**
     * @return The random number generator.
     */
    public GenerateRandom getRandom() {
        return random;
    }

    public void setRandom(final GenerateRandom random) {
        this.random = random;
    }

    /**
     * @return The best tour/path.
     */
    public int[] getBestTour() {
        return bestPath;
    }

    public double getBestCost() {
        return this.bestCost;
    }

    /**
     * @return The pheromone levels.
     */
    public double[][] getPheromone() {
        return pheromone;
    }

    /**
     * @return The cost graph.
     */
    public CostGraph getGraph() {
        return graph;
    }

    /**
     * @return The ants.
     */
    public List<DiscreteAnt> getAnts() {
        return ants;
    }

    /**
     * @return Constant that defines the attractiveness of the pheromone trail.
     */
    public double getAlpha() {
        return alpha;
    }

    /**
     * Set the constant that defines the attractiveness of the pheromone trail.
     *
     * @param alpha The constant that defines the attractiveness of the pheromone trail.
     */
    public void setAlpha(final double alpha) {
        this.alpha = alpha;
    }

    /**
     * @return Constant that defines the attractiveness of better state transitions (from one node to another).
     */
    public double getBeta() {
        return beta;
    }

    /**
     * Constant that defines the attractiveness of better state transitions (from one node to another).
     *
     * @param beta The constant that defines the attractiveness of better state transitions (from one node to another).
     */
    public void setBeta(final double beta) {
        this.beta = beta;
    }

    /**
     * @return The pheromone evaporation level.
     */
    public double getEvaporation() {
        return evaporation;
    }

    /**
     * Set the pheromone evaporation level.
     *
     * @param evaporation The pheromone evaporation level.
     */
    public void setEvaporation(final double evaporation) {
        this.evaporation = evaporation;
    }

    /**
     * @return The amount of pheromone that the nodes of a path share for a trip.
     */
    public double getQ() {
        return q;
    }

    /**
     * Set the amount of pheromone that the nodes of a path share for a trip.
     *
     * @param q The amount of pheromone that the nodes of a path share for a trip.
     */
    public void setQ(final double q) {
        this.q = q;
    }

    /**
     * @return The base probability.
     */
    public double getPr() {
        return pr;
    }

    /**
     * Set the base probability.
     *
     * @param pr The base probability.
     */
    public void setPr(final double pr) {
        this.pr = pr;
    }

    /**
     * @return The best path.
     */
    public int[] getBestPath() {
        return bestPath;
    }

}
