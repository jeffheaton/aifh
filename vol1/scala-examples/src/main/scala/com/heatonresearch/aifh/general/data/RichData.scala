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
package com.heatonresearch.aifh.general.data

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

/**
 * Support class for adding methods to our mutable and immutable vector types.
 * Importing RichData._ into a file will add methods to Vector and ArrayBuffer
 */
object RichData {

  implicit def toRichVector(v: Vector[Double]): RichVector = new RichVector(v)

  implicit def toRichBuffer[T](b: ArrayBuffer[T]): RichArrayBuffer[T] = new RichArrayBuffer(b)

}

class RichVector(data :Vector[Double]) {
  /**
   * Return the index that has the max value.
   *
   * @return The index.
   */
  def maxIndex: Int = {
    var result: Int = -1
    var max = Double.NegativeInfinity
    for(i <- 0 until data.length) {
      if (data(i) > max) {
        max = data(i)
        result = i
      }
    }
    result
  }

  def swap(p1: Int, p2: Int): Vector[Double] = {
    val temp1 = data(p1)
    val temp2 = data(p2)
    data.updated(p1,temp2).updated(p2,temp1)
  }
}

class RichArrayBuffer[T](buffer: ArrayBuffer[T]) {

  def set(values: Seq[T]) {
    buffer.clear()
    buffer ++= values
  }

  def updateValues(f: (Int, T) => T) {
    for(i <- 0 until buffer.length)
      buffer(i) = f(i,buffer(i))
  }

  def setFrom(other: IndexedSeq[T], otherStart: Int, thisStart: Int, noElements: Int) {
    for(i <- 0 until noElements)
      buffer(thisStart + i) = other(otherStart + i)
  }
}
