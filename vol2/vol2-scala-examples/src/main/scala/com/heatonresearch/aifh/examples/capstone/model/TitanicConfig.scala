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
package com.heatonresearch.aifh.examples.capstone.model

/**
 * Configuration data for the Titanic project.
 */
object TitanicConfig {
  /**
   * The name of the training data. (that we are to train on)
   */
  val TrainingFilename: String = "train.csv"
  /**
   * The name of the test data. (that Kaggle evaluates us on)
   */
  val TestFilename: String = "test.csv"
  /**
   * Dump the normalized data to this file.  This file is not actually used, but rather can be viewed to see
   * the normalization.
   */
  val NormDumpFilename: String = "normalized_dump.csv"
  /**
   * The number of input features used.
   */
  val InputFeatureCount: Int = 13
  /**
   * The low range of the normalization.
   */
  val InputNormalizeLow: Double = -1
  /**
   * The high range of the normalization.
   */
  val InputNormalizeHigh: Double = 1
  /**
   * The value used for a prediction of survival.
   */
  val PredictSurvive: Double = 1
  /**
   * The value used for a prediction of perish.
   */
  val PredictPerish: Double = 0
  /**
   * The number of folds to use.
   */
  val FoldCount: Int = 5
  /**
   * The number of particles to use.
   */
  val ParticleCount: Int = 30
  /**
   * The number of RBF functions to use in each network.
   */
  val RBF_COUNT: Int = 5
  /**
   * The number of iterations to allow with no improvement.
   */
  val AllowNoImprovement: Int = 100
}