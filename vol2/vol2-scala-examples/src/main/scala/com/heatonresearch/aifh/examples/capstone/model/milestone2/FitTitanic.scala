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

import com.heatonresearch.aifh.examples.capstone.model.TitanicConfig
import com.heatonresearch.aifh.examples.capstone.model.milestone1.NormalizeTitanic
import com.heatonresearch.aifh.examples.capstone.model.milestone1.TitanicStats
import com.heatonresearch.aifh.learning.{MLMethod, RBFNetwork, TrainPSO}
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import java.io.File
import java.io.IOException

/**
 * The second milestone for titanic is to fit and cross validate a model.
 */
object FitTitanic {
  /**
   * Main entry point.
   *
   * @param args The path to the data file.
   */
  def main(args: Array[String]) {
    var filename: String = null
    if (args.length != 1) {
      filename = System.getProperty("FILENAME")
      if (filename == null) {
        println("Please call this program with a single parameter that specifies your data directory.\n" + "If you are calling with gradle, consider:\n" + "gradle runCapstoneTitanic2 -Pdata_path=[path to your data directory]\n")
        System.exit(0)
      }
    }
    else {
      filename = args(0)
    }
    val dataPath: File = new File(filename)
    val fit: FitTitanic = new FitTitanic
    fit.process(dataPath)
  }
}

class FitTitanic {
  /**
   * Train a fold.
   *
   * @param k    The fold number.
   * @param fold The fold.
   */
  def trainFold(k: Int, fold: CrossValidateFold) {
    var noImprove: Int = 0
    var localBest: Double = 0
    val training = fold.getTrainingSet
    val validation = fold.getValidationSet
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val particles: Array[RBFNetwork] = new Array[RBFNetwork](TitanicConfig.ParticleCount)
    for(i <- 0 until particles.length) {
      particles(i) = RBFNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RBF_COUNT, 1)
      particles(i).reset(rnd)
    }
    if (bestNetwork == null) {
      bestNetwork = RBFNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RBF_COUNT, 1)
    }
    val score: ScoreFunction = new ScoreTitanic(training)
    val scoreValidate: ScoreFunction = new ScoreTitanic(validation)
    var done = false
    val train = TrainPSO(particles.asInstanceOf[Array[MLMethod]], score)
    var iterationNumber: Int = 0
    val line: StringBuilder = new StringBuilder
    do {
      iterationNumber += 1
      train.iteration()
      val best = train.getBestParticle.asInstanceOf[RBFNetwork]
      best.getLongTermMemory.clone()
      val trainingScore: Double = train.getLastError
      val validationScore: Double = scoreValidate.calculateScore(best)
      if (validationScore > bestScore) {
        System.arraycopy(best.getLongTermMemory, 0, this.bestNetwork.getLongTermMemory, 0, best.getLongTermMemory.length)
        this.bestScore = validationScore
      }
      if (validationScore > localBest) {
        noImprove = 0
        localBest = validationScore
      }
      else {
        noImprove += 1
      }
      line.setLength(0)
      line.append("Fold #")
      line.append(k + 1)
      line.append(", Iteration #")
      line.append(iterationNumber)
      line.append(": training correct: ")
      line.append(trainingScore)
      line.append(", validation correct: ")
      line.append(validationScore)
      line.append(", no improvement: ")
      line.append(noImprove)
      if (noImprove > TitanicConfig.AllowNoImprovement) {
        done = true
      }
      println(line.toString())
    } while (!done)
    fold.score = localBest
  }

  /**
   * Fit a RBF model to the titanic.
   *
   * @param dataPath The path that contains the data file.
   */
  def process(dataPath: File) {
    val trainingPath = new File(dataPath, TitanicConfig.TrainingFilename)
    val testPath = new File(dataPath, TitanicConfig.TestFilename)
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    try {
      val stats: TitanicStats = new TitanicStats
      NormalizeTitanic.analyze(stats, trainingPath)
      NormalizeTitanic.analyze(stats, testPath)
      val training = NormalizeTitanic.normalize(stats, trainingPath, null, TitanicConfig.InputNormalizeLow, TitanicConfig.InputNormalizeHigh, TitanicConfig.PredictSurvive, TitanicConfig.PredictPerish)
      this.cross = new CrossValidate(TitanicConfig.FoldCount, training, rnd)
      for(k <- 0 until cross.size) {
        println("Cross validation fold #" + (k + 1) + "/" + cross.size)
        trainFold(k, cross.folds.get(k))
      }
      println("Crossvalidation summary:")
      var k: Int = 1
      import scala.collection.JavaConversions._
      for (fold <- cross.folds) {
        println("Fold #" + k + ": " + fold.score)
        k += 1
      }
      System.out.print("Final, crossvalidated score:" + cross.getScore)
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }

  /**
   * @return The best network from the folds.
   */
  def getBestNetwork: RBFNetwork = bestNetwork

  /**
   * @return The cross validation folds.
   */
  def getCrossvalidation: CrossValidate = cross

  /**
   * The best RBF network.
   */
  private var bestNetwork: RBFNetwork = null
  /**
   * The best score.
   */
  private var bestScore: Double = .0
  /**
   * The cross validation folds.
   */
  private var cross: CrossValidate = null
}