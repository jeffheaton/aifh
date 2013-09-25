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
package com.heatonresearch.aifh.examples.kmeans

import com.heatonresearch.aifh.kmeans.KMeans
import com.heatonresearch.aifh.normalize.DataSet
import java.io.InputStream

/**
 * Try to cluster the Iris data set.
 */
object PerformCluster extends App {
  (new PerformCluster).run()
}

class PerformCluster {
  /**
   * Perform the example.
   */
  def run() {
    try {
      val istream: InputStream = this.getClass.getResourceAsStream("/iris.csv")
      val ds = DataSet.load(istream)
      istream.close()
      val observations = ds.extractUnsupervisedLabeled(4)
      val kmeans = new KMeans(3)
      kmeans.initForgy(observations)
      val iterations = kmeans.iteration(1000)
      println("Finished after " + iterations + " iterations.")
      for(i <- 0 until kmeans.k) {
        val cluster = kmeans.getClusters(i)
        println("* * * Cluster #" + i)
        for (d <- cluster.getObservations) {
          println(d.toString)
        }
      }
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }
}