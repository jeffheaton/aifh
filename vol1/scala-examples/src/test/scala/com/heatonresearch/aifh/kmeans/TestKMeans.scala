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
import com.heatonresearch.aifh.distance.EuclideanDistance
import com.heatonresearch.aifh.general.data.BasicData
import com.heatonresearch.aifh.randomize.BasicGenerateRandom
import org.scalatest.Suite

/**
 * Test KMeans.
 */
object TestKMeans {
  def getDataSet: Vector[BasicData] = {
    Vector[BasicData](
      new BasicData(Vector(0.0, 0.0), "a"),
      new BasicData(Vector(0.0, 1.0), "a"),
      new BasicData(Vector(1.0, 0.0), "a"),
      new BasicData(Vector(100.0, 100.0), "b"),
      new BasicData(Vector(99.0, 100.0), "b"),
      new BasicData(Vector(100.0, 99.0), "b"),
      new BasicData(Vector(0.0, 100.0), "c"),
      new BasicData(Vector(1.0, 100.0), "c"),
      new BasicData(Vector(0.0, 99.0), "c"),
      new BasicData(Vector(100.0, 0.0), "d"),
      new BasicData(Vector(100.0, 1.0), "d"),
      new BasicData(Vector(99.0, 0.0), "d"))
  }
}

class TestKMeans extends Suite {
  import TestKMeans.getDataSet

  def testClusterForgy() {
    val kmeans = new KMeans(4)
    kmeans.randomGeneration = new BasicGenerateRandom(22)
    kmeans.initForgy(getDataSet)
    val iterations = kmeans.iteration(1000)
    assert(3 === iterations)
    val cluster1 = kmeans.getClusters(0)
    val cluster2 = kmeans.getClusters(1)
    val cluster3 = kmeans.getClusters(2)
    val cluster4 = kmeans.getClusters(3)
    assert(3 === cluster1.noObservations)
    assert(3 === cluster2.noObservations)
    assert(3 === cluster3.noObservations)
    assert(3 === cluster4.noObservations)
  }

  def testClusterRandom() {
    val kmeans = new KMeans(4)
    kmeans.randomGeneration = new BasicGenerateRandom(22)
    kmeans.initRandom(getDataSet)
    val iterations = kmeans.iteration(1000)
    assert(4 === iterations)
    val cluster1 = kmeans.getClusters(0)
    val cluster2 = kmeans.getClusters(1)
    val cluster3 = kmeans.getClusters(2)
    val cluster4 = kmeans.getClusters(3)
    assert(3 === cluster1.noObservations)
    assert(3 === cluster2.noObservations)
    assert(3 === cluster3.noObservations)
    assert(3 === cluster4.noObservations)
  }

  def testGeneral() {
    val kmeans = new KMeans(5)
    assert(5 === kmeans.k)
    kmeans.randomGeneration = new BasicGenerateRandom()
    kmeans.distanceMetric = new EuclideanDistance
    assert(true === kmeans.randomGeneration.isInstanceOf[BasicGenerateRandom])
    assert(true === kmeans.distanceMetric.isInstanceOf[EuclideanDistance])
  }

  def testTooManyClusters() {
    val kmeans = new KMeans(13)
    intercept[AIFHError] {
      kmeans.initRandom(getDataSet)
    }
  }

  def testEarlyIteration() {
    val kmeans = new KMeans(3)
    intercept[AIFHError] {
      kmeans.iteration
    }
  }

  def testNoObservations() {
    val list = Vector[BasicData]()
    val kmeans = new KMeans(3)
    intercept[AIFHError] {
      kmeans.initForgy(list)
    }
  }

  def testNoDimension() {
    val list = Vector(new BasicData(0))
    val kmeans = new KMeans(3)
    intercept[AIFHError] {
      kmeans.initForgy(list)
    }
  }

  def testMaxClusters() {
    val kmeans = new KMeans(12)
    kmeans.randomGeneration = new BasicGenerateRandom(22)
    kmeans.initRandom(getDataSet)
    val iterations: Int = kmeans.iteration(1000)
    assert(1 === iterations)
  }
}
