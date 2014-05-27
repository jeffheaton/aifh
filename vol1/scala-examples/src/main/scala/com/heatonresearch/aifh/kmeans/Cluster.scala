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

import com.heatonresearch.aifh.general.data.BasicData
import scala.collection.mutable.ArrayBuffer

/**
 * A cluster of observations. All observations must have the same number of dimensions.
 *
 * Construct a cluster with the specified number of dimensions.
 *
 * @param theDimensions The number of dimensions.
 */
class Cluster(theDimensions: Int) {
  /**
   * The observations in this cluster.
   */
  private var observations = Vector[BasicData]()
  /**
   * The center of these observations.
   */
  private var center: Vector[Double] = Vector.fill(theDimensions)(0.0)

  def removeObservation(obs: BasicData) {
    observations = observations.filterNot(_ == obs)
  }

  def noObservations = observations.size

  def addObservation(observation: BasicData) {
    observations = observations :+ observation
  }

  /**
   * Get the number of dimensions.
   *
   * @return The number of dimensions.
   */
  def getDimensions: Int = center.size

  /**
   * @return The center of the observations.
   */
  def getCenter: Vector[Double] = center

  def setCenter(newCenter: Vector[Double]) {
    center = newCenter
  }

  def getObservation(index: Int): BasicData = observations(index)

  def removeObservation(index: Int): BasicData = {
    val res = observations(index)
    removeObservation(res)
    res
  }

  /**
   * @return The observations in this cluster.
   */
  def getObservations: Vector[BasicData] = observations

  /**
   * Calculate the center (or mean) of the observations.
   */
  def calculateCenter() {
    val newValues = ArrayBuffer.fill(theDimensions)(0.0)

    for (observation <- observations) {
      for(i <- 0 until theDimensions) {
        newValues(i) += observation.input(i)
      }
    }

    for(i <- 0 until theDimensions) {
      newValues(i) /= observations.size
    }
    
    center = newValues.toVector
  }

  override def toString: String = {
    s"[Cluster: dimensions=$getDimensions, observations=${observations.size}, center=$center]"
  }

}
