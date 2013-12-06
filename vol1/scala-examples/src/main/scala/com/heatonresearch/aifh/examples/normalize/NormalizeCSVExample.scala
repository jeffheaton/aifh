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
package com.heatonresearch.aifh.examples.normalize

import com.heatonresearch.aifh.normalize.DataSet
import java.io.File

/**
 * A simple normalization example for the Iris data set.
 */
object NormalizeCSVExample extends App {
  (new NormalizeCSVExample).run()
}

class NormalizeCSVExample {
  def run() {
    try {
      val iStream = this.getClass.getResourceAsStream("/iris.csv")
      val ds = DataSet.load(iStream)
      iStream.close()
      ds.normalizeRange(0, 0, 1)
      ds.normalizeRange(1, 0, 1)
      ds.normalizeRange(2, 0, 1)
      ds.normalizeRange(3, 0, 1)
      ds.encodeEquilateral(4)
      val outputFile = new File("normalized.csv")
      DataSet.save(outputFile, ds)
      println(s"Output written to: ${outputFile.getAbsolutePath}")
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }
}