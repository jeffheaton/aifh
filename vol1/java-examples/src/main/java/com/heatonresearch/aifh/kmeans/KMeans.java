package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.randomize.BasicGenerateRandom;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * KMeans Clustering.
 */
public class KMeans {

    /**
     * The number of clusters.
     */
    private final int k;

    /**
     * The clusters.
     */
    private final List<Cluster> clusters = new ArrayList<Cluster>();

    /**
     * The random number generator to use.
     */
    private GenerateRandom randomGeneration = new BasicGenerateRandom();

    /**
     * The
     */
    private CalculateDistance distanceMetric = new EuclideanDistance();

    public KMeans(final int theK) {
        this.k = theK;
    }

    /**
     * Validate and find the number of dimensions from the first observation.
     *
     * @param theObservations The observations.
     * @return The number of dimensions.
     */
    private int findDimensions(final List<BasicData> theObservations) {
        if (theObservations.size() == 0) {
            throw new AIFHError("No observations provided to cluster, array zero length.");
        }

        if (theObservations.size() < this.k) {
            throw new AIFHError("There are fewer observations ("
                    + theObservations.size() + ") than k (" + this.k + ").");
        }

        int dimensions = theObservations.get(0).getInput().length;

        if (dimensions == 0) {
            throw new AIFHError("Observations have no dimensions.");
        }

        return dimensions;
    }

    public void initRandom(final List<BasicData> theObservations) {
        int dimensions = findDimensions(theObservations);

        // create the clusters
        for (int i = 0; i < this.k; i++) {
            this.clusters.add(new Cluster(dimensions));
        }

        // assign each observation to a random cluster
        for (BasicData observation : theObservations) {
            int clusterIndex = this.randomGeneration.nextInt(this.k);
            Cluster cluster = this.clusters.get(clusterIndex);
            cluster.getObservations().add(observation);
        }

        // handle any empty clusters
        for (Cluster cluster : this.clusters) {
            if (cluster.getObservations().size() == 0) {
                boolean done = false;
                while (!done) {
                    int sourceIndex = this.randomGeneration.nextInt(this.k);
                    Cluster source = this.clusters.get(sourceIndex);
                    if (source != cluster && source.getObservations().size() > 1) {
                        int sourceObservationIndex = this.randomGeneration.nextInt(source.getObservations().size());
                        BasicData sourceObservation = source.getObservations().get(sourceObservationIndex);
                        source.getObservations().remove(sourceObservationIndex);
                        cluster.getObservations().add(sourceObservation);
                        done = true;
                    }
                }
            }
        }

        // calculate initial centers
        updateStep();

    }

    public void initForgy(final List<BasicData> theObservations) {
        int dimensions = findDimensions(theObservations);

        this.clusters.clear();

        Set<Integer> usedObservations = new HashSet<Integer>();

        for (int i = 0; i < this.k; i++) {
            Cluster cluster = new Cluster(dimensions);
            this.clusters.add(cluster);

            int observationIndex = -1;

            while (observationIndex == -1) {
                observationIndex = this.randomGeneration.nextInt(theObservations.size());
                if (usedObservations.contains(observationIndex)) {
                    observationIndex = -1;
                }
            }

            double[] observation = theObservations.get(observationIndex).getInput();
            System.arraycopy(observation, 0, cluster.getCenter(), 0, dimensions);
            usedObservations.add(observationIndex);
        }

        // assign all observations to a cluster
        for (BasicData observation : theObservations) {
            Cluster cluster = findNearestCluster(observation.getInput());
            cluster.getObservations().add(observation);
        }

        // calculate initial centers
        updateStep();
    }

    private void updateStep() {
        for (Cluster cluster : clusters) {
            cluster.calculateCenter();
        }
    }

    private boolean assignmentStep() {
        boolean done = true;

        for (Cluster cluster : this.clusters) {
            int observationIndex = 0;
            int observationCount = cluster.getObservations().size();

            if (observationCount > 1) {
                while (observationIndex < observationCount) {
                    BasicData observation = cluster.getObservations().get(observationIndex++);

                    Cluster targetCluster = findNearestCluster(observation.getInput());
                    if (targetCluster != cluster) {
                        cluster.getObservations().remove(observation);
                        targetCluster.getObservations().add(observation);
                        observationCount--;
                        done = false;
                    }
                }
            }
        }

        return done;
    }

    public Cluster findNearestCluster(final double[] observation) {
        Cluster result = null;
        double resultDist = Double.POSITIVE_INFINITY;

        for (Cluster cluster : this.clusters) {
            double dist = this.distanceMetric.calculate(observation, cluster.getCenter());
            if (dist < resultDist) {
                resultDist = dist;
                result = cluster;
            }
        }

        return result;
    }

    public boolean iteration() {
        if (this.clusters.size() == 0) {
            throw new AIFHError("Must call one of the init methods first.");
        }

        boolean done = assignmentStep();

        if (!done) {
            updateStep();
        }

        return done;
    }

    public int iteration(int maxIterations) {
        int iterationCount = 1;

        while (iterationCount <= maxIterations && !iteration()) {
            iterationCount++;
        }

        return iterationCount;
    }

    public GenerateRandom getRandomGeneration() {
        return randomGeneration;
    }

    public void setRandomGeneration(final GenerateRandom randomGeneration) {
        this.randomGeneration = randomGeneration;
    }

    public CalculateDistance getDistanceMetric() {
        return distanceMetric;
    }

    public void setDistanceMetric(final CalculateDistance distanceMetric) {
        this.distanceMetric = distanceMetric;
    }

    public int getK() {
        return k;
    }

    public List<Cluster> getClusters() {
        return clusters;
    }
}
