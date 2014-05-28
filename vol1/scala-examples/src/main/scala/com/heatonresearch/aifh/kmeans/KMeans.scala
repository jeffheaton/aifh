/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Scala Version
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
package com.heatonresearch.aifh.kmeans

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.distance.{CalculateDistance, EuclideanDistance}
import com.heatonresearch.aifh.general.data.BasicData
import com.heatonresearch.aifh.randomize.BasicGenerateRandom
import com.heatonresearch.aifh.randomize.GenerateRandom
import scala.collection.mutable.ListBuffer

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

 * @param k The number of clusters (K).
 */
class KMeans(val k: Int) {

  /**
   * The clusters.
   */
  private val clusters = ListBuffer[Cluster]()
  /**
   * The random number generator to use.
   */
  var randomGeneration: GenerateRandom = new BasicGenerateRandom()
  /**
   * The
   */
  var distanceMetric: CalculateDistance = new EuclideanDistance


  /**
   * Validate and find the number of dimensions from the first observation.
   *
   * @param theObservations The observations.
   * @return The number of dimensions.
   */
  private def findDimensions(theObservations: Vector[BasicData]): Int = {
    if (theObservations.isEmpty) {
      throw new AIFHError("No observations provided to cluster, array zero length.")
    }
    if (theObservations.size < k) {
      throw new AIFHError("There are fewer observations (" + theObservations.size + ") than k (" + k + ").")
    }
    val dimensions: Int = theObservations(0).input.length
    if (dimensions == 0) {
      throw new AIFHError("Observations have no dimensions.")
    }    
    dimensions
  }

  /**
   * Init the observations to random clusters.  Use the "Init Random" algorithm. The Random Partition method first
   * randomly assigns a cluster to each observation and then proceeds to the update step, thus computing the initial mean to be the centroid of the cluster's randomly assigned points.
   *
   * @param theObservations The observations to cluster.
   */
  def initRandom(theObservations: Vector[BasicData]) {
    val dimensions = findDimensions(theObservations)
    for(i <- 0 until k) {
      clusters += new Cluster(dimensions)
    }
    for (observation <- theObservations) {
      val clusterIndex = randomGeneration.nextInt(k)
      val cluster = clusters(clusterIndex)
      cluster.addObservation(observation)
    }
    for (cluster <- clusters) {
      if (cluster.noObservations == 0) {
        var done = false
        while (!done) {
          val sourceIndex = randomGeneration.nextInt(k)
          val source = clusters(sourceIndex)
          if ((source != cluster) && source.noObservations > 1) {
            val sourceObservationIndex = randomGeneration.nextInt(source.noObservations)
            val sourceObservation = source.getObservation(sourceObservationIndex)
            source.removeObservation(sourceObservationIndex)
            cluster.addObservation(sourceObservation)
            done = true
          }
        }
      }
    }
    updateStep()
  }

  /**
   * Init the observations to random clusters.  The Forgy method randomly chooses k observations from the
   * data set and uses these as the initial means.
   *
   * @param theObservations The observations to cluster.
   */
  def initForgy(theObservations: Vector[BasicData]) {
    val dimensions: Int = findDimensions(theObservations)
    clusters.clear()
    var usedObservations = Set.empty[Integer]
    for(i <- 0 until k) {
      val cluster = new Cluster(dimensions)
      clusters += cluster
      var observationIndex: Int = -1
      while (observationIndex == -1) {
        observationIndex = randomGeneration.nextInt(theObservations.size)
        if (usedObservations.contains(observationIndex)) {
          observationIndex = -1
        }
      }
      val observation = theObservations(observationIndex).input
      cluster.setCenter(observation)
      usedObservations += observationIndex
    }
    for (observation <- theObservations) {
      val cluster: Cluster = findNearestCluster(observation.input)
      cluster.addObservation(observation)
    }
    updateStep()
  }

  /**
   * The update step updates the centroids.
   */
  private def updateStep() {
    for (cluster <- clusters) {
      cluster.calculateCenter()
    }
  }

  /**
   * The assignment step assigns observations to the nearest clusters.
   *
   * @return True, if we are done.  We are done if no observations moved clusters.
   */
  private def assignmentStep: Boolean = {
    var done: Boolean = true
    for (cluster <- clusters) {
      var observationIndex: Int = 0
      var observationCount = cluster.noObservations
      if (observationCount > 1) {
        while (observationIndex < observationCount) {
          val observation: BasicData = cluster.getObservation(observationIndex)
          observationIndex += 1
          val targetCluster: Cluster = findNearestCluster(observation.input)
          if (targetCluster ne cluster) {
            cluster.removeObservation(observation)
            targetCluster.addObservation(observation)
            observationCount -= 1
            done = false
          }
        }
      }
    }
    done
  }

  /**
   * Find the nearest cluster for an observation.
   *
   * @param observation The observation.
   * @return The nearest cluster.
   */
  def findNearestCluster(observation: Vector[Double]): Cluster = {
    var result: Cluster = null
    var resultDist: Double = Double.PositiveInfinity
    for (cluster <- clusters) {
      val dist: Double = distanceMetric.calculate(observation, cluster.getCenter)
      if (dist < resultDist) {
        resultDist = dist
        result = cluster
      }
    }
    result
  }

  /**
   * Perform one iteration of assignment and update steps.
   *
   * @return True, if we are done, no new assignments.
   */
  def iteration: Boolean = {
    if (clusters.isEmpty) {
      throw new AIFHError("Must call one of the init methods first.")
    }
    val done: Boolean = assignmentStep
    if (!done) {
      updateStep()
    }
    done
  }

  /**
   * Perform the specified number of iterations. Stop early if we are done.
   *
   * @param maxIterations The max number of iterations.
   * @return True, if we are done.
   */
  def iteration(maxIterations: Int): Int = {
    var iterationCount: Int = 1
    while (iterationCount <= maxIterations && !iteration) {
      iterationCount += 1
    }
    iterationCount
  }

  /**
   * @return The clusters.
   */
  def getClusters: List[Cluster] = clusters.toList
}
