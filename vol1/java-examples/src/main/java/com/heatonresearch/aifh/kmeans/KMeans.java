/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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
 * KMeans Clustering.  First, observations are each placed into random clusters.  There are two methods to do this:
 * random and Forgy. Then we iterate through assignment and update steps.  Assignment places clusters in new clusters
 * that they might be closer to.  Update updates the center of each cluster, called the centroid.  The center of each
 * cluster is the mean of all observations in that cluster.
 * <p/>
 * This class uses a number of supporting objects:
 * <p/>
 * randomGeneration: The random number generator used for clustering.
 * distanceMetric: The distance metric used to determine distance to centroids.
 * <p/>
 * http://en.wikipedia.org/wiki/Kmeans
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

    /**
     * Construct the object with K clusters.
     *
     * @param theK The number of clusters (K).
     */
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

        final int dimensions = theObservations.get(0).getInput().length;

        if (dimensions == 0) {
            throw new AIFHError("Observations have no dimensions.");
        }

        return dimensions;
    }

    /**
     * Init the observations to random clusters.  Use the "Init Random" algorithm. The Random Partition method first
     * randomly assigns a cluster to each observation and then proceeds to the update step, thus computing the initial mean to be the centroid of the cluster's randomly assigned points.
     *
     * @param theObservations The observations to cluster.
     */
    public void initRandom(final List<BasicData> theObservations) {
        final int dimensions = findDimensions(theObservations);

        // create the clusters
        for (int i = 0; i < this.k; i++) {
            this.clusters.add(new Cluster(dimensions));
        }

        // assign each observation to a random cluster
        for (final BasicData observation : theObservations) {
            final int clusterIndex = this.randomGeneration.nextInt(this.k);
            final Cluster cluster = this.clusters.get(clusterIndex);
            cluster.getObservations().add(observation);
        }

        // handle any empty clusters
        for (final Cluster cluster : this.clusters) {
            if (cluster.getObservations().size() == 0) {
                boolean done = false;
                while (!done) {
                    final int sourceIndex = this.randomGeneration.nextInt(this.k);
                    final Cluster source = this.clusters.get(sourceIndex);
                    if (source != cluster && source.getObservations().size() > 1) {
                        final int sourceObservationIndex = this.randomGeneration.nextInt(source.getObservations().size());
                        final BasicData sourceObservation = source.getObservations().get(sourceObservationIndex);
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

    /**
     * Init the observations to random clusters.  The Forgy method randomly chooses k observations from the
     * data set and uses these as the initial means.
     *
     * @param theObservations The observations to cluster.
     */

    public void initForgy(final List<BasicData> theObservations) {
        final int dimensions = findDimensions(theObservations);

        this.clusters.clear();

        final Set<Integer> usedObservations = new HashSet<Integer>();

        for (int i = 0; i < this.k; i++) {
            final Cluster cluster = new Cluster(dimensions);
            this.clusters.add(cluster);

            int observationIndex = -1;

            while (observationIndex == -1) {
                observationIndex = this.randomGeneration.nextInt(theObservations.size());
                if (usedObservations.contains(observationIndex)) {
                    observationIndex = -1;
                }
            }

            final double[] observation = theObservations.get(observationIndex).getInput();
            System.arraycopy(observation, 0, cluster.getCenter(), 0, dimensions);
            usedObservations.add(observationIndex);
        }

        // assign all observations to a cluster
        for (final BasicData observation : theObservations) {
            final Cluster cluster = findNearestCluster(observation.getInput());
            cluster.getObservations().add(observation);
        }

        // calculate initial centers
        updateStep();
    }

    /**
     * The update step updates the centroids.
     */
    private void updateStep() {
        for (final Cluster cluster : clusters) {
            cluster.calculateCenter();
        }
    }

    /**
     * The assignment step assigns observations to the nearest clusters.
     *
     * @return True, if we are done.  We are done if no observations moved clusters.
     */
    private boolean assignmentStep() {
        boolean done = true;

        for (final Cluster cluster : this.clusters) {
            int observationIndex = 0;
            int observationCount = cluster.getObservations().size();

            if (observationCount > 1) {
                while (observationIndex < observationCount) {
                    final BasicData observation = cluster.getObservations().get(observationIndex++);

                    final Cluster targetCluster = findNearestCluster(observation.getInput());
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

    /**
     * Find the nearest cluster for an observation.
     *
     * @param observation The observation.
     * @return The nearest cluster.
     */
    public Cluster findNearestCluster(final double[] observation) {
        Cluster result = null;
        double resultDist = Double.POSITIVE_INFINITY;

        for (final Cluster cluster : this.clusters) {
            final double dist = this.distanceMetric.calculate(observation, cluster.getCenter());
            if (dist < resultDist) {
                resultDist = dist;
                result = cluster;
            }
        }

        return result;
    }

    /**
     * Perform one iteration of assignment and update steps.
     *
     * @return True, if we are done, no new assignments.
     */
    public boolean iteration() {
        if (this.clusters.size() == 0) {
            throw new AIFHError("Must call one of the init methods first.");
        }

        final boolean done = assignmentStep();

        if (!done) {
            updateStep();
        }

        return done;
    }

    /**
     * Perform the specified number of iterations. Stop early if we are done.
     *
     * @param maxIterations The max number of iterations.
     * @return True, if we are done.
     */
    public int iteration(final int maxIterations) {
        int iterationCount = 1;

        while (iterationCount <= maxIterations && !iteration()) {
            iterationCount++;
        }

        return iterationCount;
    }

    /**
     * @return The random number generator used.
     */
    public GenerateRandom getRandomGeneration() {
        return randomGeneration;
    }

    /**
     * Set the random number generator to use.
     *
     * @param randomGeneration The random generator to use.
     */
    public void setRandomGeneration(final GenerateRandom randomGeneration) {
        this.randomGeneration = randomGeneration;
    }

    /**
     * @return The distance metric used.
     */
    public CalculateDistance getDistanceMetric() {
        return distanceMetric;
    }

    /**
     * Set the distance metric to use.
     *
     * @param distanceMetric The distance metric.
     */
    public void setDistanceMetric(final CalculateDistance distanceMetric) {
        this.distanceMetric = distanceMetric;
    }

    /**
     * @return The number of clusters.
     */
    public int getK() {
        return k;
    }

    /**
     * @return The clusters.
     */
    public List<Cluster> getClusters() {
        return clusters;
    }
}
