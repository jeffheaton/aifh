package com.heatonresearch.aifh.aco;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/7/14
 * Time: 4:40 AM
 * To change this template use File | Settings | File Templates.
 */
public class DiscreteACO {

    private final double[][] pheromone;
    private final CostGraph graph;
    private final List<DiscreteAnt> ants = new ArrayList<DiscreteAnt>();
    private double c = 1.0;
    private double alpha = 1;
    private double beta = 5;
    private double evaporation = 0.5;
    private double Q = 500;
    private double pr = 0.01;
    private GenerateRandom random = new MersenneTwisterGenerateRandom();
    private final int[] bestPath;
    private double bestCost;

    public DiscreteACO(CostGraph theGraph, int theAntCount) {
        int len = theGraph.graphSize();
        this.graph = theGraph;
        this.pheromone = new double[len][len];
        this.bestPath = new int[len];
        this.bestCost = Double.POSITIVE_INFINITY;

        for (int i = 0; i < len; i++) {
            for (int j = 0; j < len; j++) {
                this.pheromone[i][j] = c;
            }
        }

        for(int i=0;i<theAntCount;i++) {
            ants.add(new DiscreteAnt(len));
        }

    }

    private double[] calculateProbability(int currentIndex, DiscreteAnt ant) {
        double[] result = new double[this.graph.graphSize()];
        int i = ant.getPath()[currentIndex-1];

        double d = 0.0;
        for (int l = 0; l < this.graph.graphSize(); l++)
            if (!ant.wasVisited(l))
                d += Math.pow(this.pheromone[i][l], alpha)
                        * Math.pow(1.0 / this.graph.cost(i,l), beta);


        for (int j = 0; j < this.graph.graphSize(); j++) {
            if (ant.wasVisited(j)) {
                result[j] = 0.0;
            } else {
                double n = Math.pow(this.pheromone[i][j], alpha)
                        * Math.pow(1.0 / this.graph.cost(i,j), beta);
                result[j] = n / d;
            }
        }
        return result;

    }

    private int pickNextNode(int currentIndex, DiscreteAnt ant) {
        if (currentIndex==0 || this.random.nextDouble() < pr) {
            int index;
            do {
                index = this.random.nextInt(0,this.graph.graphSize());
            } while(ant.wasVisited(index));
            return index;
        }

        double[] prob = calculateProbability(currentIndex,ant);

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

    private void updatePheromone() {
        // Cause evaporation.
        for (int i = 0; i < this.pheromone.length; i++)
            for (int j = 0; j < this.pheromone[i].length; j++)
                this.pheromone[i][j] *= evaporation;

        // Adjust for ants.
        for (DiscreteAnt a : ants) {
            double d = Q / a.calculateCost(this.graph.graphSize(), this.graph);
            for (int i = 0; i < this.graph.graphSize() - 1; i++) {
                this.pheromone[a.getPath()[i]][a.getPath()[i + 1]] += d;
            }
            this.pheromone[a.getPath()[this.graph.graphSize() - 1]][a.getPath()[0]] += d;
        }
    }

    private void march() {
        for(int currentIndex = 0;currentIndex<this.graph.graphSize();currentIndex++) {
            for(DiscreteAnt a: this.ants) {
                int next = pickNextNode(currentIndex,a);
                a.visit(currentIndex, next);
            }
        }
    }

    private void setupAnts() {
        for(DiscreteAnt a: this.ants) {
            a.clear();
        }
    }

    private void updateBest() {
        int[] bestPathFound = null;

        for(DiscreteAnt a: this.ants) {
            double cost = a.calculateCost(this.graph.graphSize(), this.graph);
            if( cost<this.bestCost ) {
                bestPathFound = a.getPath();
                this.bestCost = cost;
            }
        }

        if( bestPathFound!=null ) {
            System.arraycopy(bestPathFound,0,this.bestPath,0,this.bestPath.length);
        }
    }


    public void iteration() {
        setupAnts();
        march();
        updatePheromone();
        updateBest();
    }

    public GenerateRandom getRandom() {
        return random;
    }

    public void setRandom(final GenerateRandom random) {
        this.random = random;
    }

    public int[] getBestTour() {
        return bestPath;
    }

    public double getBestCost() {
        return this.bestCost;
    }
}
