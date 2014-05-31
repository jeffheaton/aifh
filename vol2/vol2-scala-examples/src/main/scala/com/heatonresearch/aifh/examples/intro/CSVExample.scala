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
package com.heatonresearch.aifh.examples.intro

import au.com.bytecode.opencsv.CSVReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.util

/**
 * Shows how to read a CSV file.
 */
object CSVExample extends App {
  (new CSVExample).run()
}

class CSVExample {
  /**
   * Run the example.
   */
  def run() {
    var reader: CSVReader = null
    try {
      val iStream: InputStream = this.getClass.getResourceAsStream("/iris.csv")
      reader = new CSVReader(new InputStreamReader(iStream))

      var nextLine: Array[String] = null
      do {
        nextLine = reader.readNext()
        if(nextLine != null)
          println(util.Arrays.toString(nextLine.asInstanceOf[Array[Object]]))
      } while(nextLine != null)
    } catch {
      case ex: IOException =>
        ex.printStackTrace()
    } finally {
      if (reader != null) {
        try {
          reader.close()
        } catch {
          case ex: IOException =>
            ex.printStackTrace()
        }
      }
    }
  }
}