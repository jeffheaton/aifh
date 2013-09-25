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
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

/**
 * Test the cluster class.
 */
class TestCluster extends Suite with ShouldMatchers {
  def testDimensions() {
    val cluster = new Cluster(3)
    assert(true === (cluster.toString.length > 0))
    assert(3 === cluster.getDimensions)
  }

  def testCenter() {
    val cluster = new Cluster(3)
    val ob1 = Vector(2.0, 10.0, 100.0)
    val ob2 = Vector(4.0, 20.0, 200.0)
    val ob3 = Vector(6.0, 30.0, 300.0)
    cluster.addObservation(new BasicData(ob1))
    cluster.addObservation(new BasicData(ob2))
    cluster.addObservation(new BasicData(ob3))
    assert(3 === cluster.noObservations)
    cluster.calculateCenter()

    cluster.getCenter(0) should be (4.0 plusOrMinus 0.00001)
    cluster.getCenter(1) should be (20.0 plusOrMinus 0.00001)
    cluster.getCenter(2) should be (200.0 plusOrMinus 0.00001)
  }
}