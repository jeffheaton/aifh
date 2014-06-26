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
package com.heatonresearch.aifh.examples.capstone.model.milestone2

import com.heatonresearch.aifh.general.data.BasicData

/**
 * A cross validation fold.  This contains a training and validation set.  A score is also
 * held for the validation sets.
 */
class CrossValidateFold {
  /**
   * @return The training set.
   */
  def getTrainingSet: java.util.List[BasicData] = trainingSet

  /**
   * @return The validation set.
   */
  def getValidationSet: java.util.List[BasicData] = validationSet

  /**
   * The training set.
   */
  private val trainingSet: java.util.List[BasicData] = new java.util.ArrayList[BasicData]
  /**
   * The validation set.
   */
  private val validationSet: java.util.List[BasicData] = new java.util.ArrayList[BasicData]
  /**
   * The score.
   */
  var score: Double = .0
}