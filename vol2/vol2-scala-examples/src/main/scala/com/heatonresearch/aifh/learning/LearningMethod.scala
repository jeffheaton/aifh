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
package com.heatonresearch.aifh.learning

/**
 * A learning method is used to train a machine learning algorithm to better classify or perform regression on
 * input data.
 */
trait LearningMethod {
  /**
   * Perform one training iteration.
   */
  def iteration()

  /**
   * @return The error from the last training iteration.
   */
  def getLastError: Double

  /**
   * @return True, if we are done learning.  Not all learning algorithms know when they are done, in this case
   *         false is always returned.
   */
  def done: Boolean

  /**
   * @return A string that indicates the status of training.
   */
  def getStatus: String

  /**
   * Should be called after the last iteration to make sure training completes any final tasks.
   */
  def finishTraining()
}