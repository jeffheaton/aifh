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
package com.heatonresearch.aifh.general.collections

import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.RandomChoice
import java.io.Serializable

/**
 * This class is used to choose between several objects with a specified probability.
 *
 * @tparam T The type of object to choose from.
 */
class ChooseObject[T] extends Serializable {
  /**
   * Finalize the structure and set the probabilities.
   */
  def finalizeStructure() {
    import scala.collection.JavaConversions._

    val d = list.map(_.getProbability).toArray
    this.chooser = new RandomChoice(d)
  }

  /**
   * Add an object.
   *
   * @param probability The probability to choose this object.
   * @param opp         The object to add.
   */
  def add(probability: Double, opp: T) {
    list.add(new ObjectHolder[T](opp, probability))
  }

  /**
   * @return The number of objects added.
   */
  def size: Int = list.size

  /**
   * Choose a random object.
   *
   * @param theGenerator  Random number generator.
   * @return The random choice.
   */
  def pick(theGenerator: GenerateRandom): T = {
    val index = chooser.generate(theGenerator)
    list.get(index).getObj
  }

  /**
   * @return The object to choose from.
   */
  def getList: java.util.List[ObjectHolder[T]] = list

  /**
   * CLear all objects from the collection.
   */
  def clear() {
    list.clear()
  }

  /**
   * @return The first object in the list.
   */
  def pickFirst: T = list.get(0).getObj

  /**
   * The objects that we are choosing from.
   */
  private val list: java.util.List[ObjectHolder[T]] = new java.util.ArrayList[ObjectHolder[T]]
  /**
   * The random choose.
   */
  private var chooser: RandomChoice = null
}