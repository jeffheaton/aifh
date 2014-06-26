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
package com.heatonresearch.aifh.examples.capstone.model.milestone3

import au.com.bytecode.opencsv.CSVWriter
import com.heatonresearch.aifh.examples.capstone.model.TitanicConfig
import com.heatonresearch.aifh.examples.capstone.model.milestone1.NormalizeTitanic
import com.heatonresearch.aifh.examples.capstone.model.milestone1.TitanicStats
import com.heatonresearch.aifh.examples.capstone.model.milestone2.CrossValidate
import com.heatonresearch.aifh.examples.capstone.model.milestone2.CrossValidateFold
import com.heatonresearch.aifh.examples.capstone.model.milestone2.FitTitanic
import com.heatonresearch.aifh.learning.RBFNetwork
import java.io._
import java.text.SimpleDateFormat
import java.util.Date

/**
 * The final titanic milestone.  We use the test data from Kaggle and prepare a submission.
 */
object SubmitTitanic {
  /**
   * The main method.
   *
   * @param args The path to the data file.
   */
  def main(args: Array[String]) {
    var filename: String = null
    if (args.length != 1) {
      filename = System.getProperty("FILENAME")
      if (filename == null) {
        println("Please call this program with a single parameter that specifies your data directory.\n" + "If you are calling with gradle, consider:\n" + "gradle runCapstoneTitanic3 -Pdata_path=[path to your data directory]\n")
        System.exit(0)
      }
    } else {
      filename = args(0)
    }
    val dataPath = new File(filename)
    val fit = new FitTitanic
    fit.process(dataPath)
    val bestNetwork = fit.getBestNetwork
    val submit = new SubmitTitanic
    submit.submit(dataPath, bestNetwork, fit.getCrossvalidation)
  }
}

class SubmitTitanic {
  /**
   * Prepare a Kaggle submission for Titanic.
   *
   * @param dataPath    The data path.
   * @param bestNetwork The best network.
   * @param cross       The cross validated data.
   */
  def submit(dataPath: File, bestNetwork: RBFNetwork, cross: CrossValidate) {
    try {
      val now: String = new SimpleDateFormat("yyyyMMddhhmm").format(new Date)
      val trainingPath = new File(dataPath, TitanicConfig.TrainingFilename)
      val testPath = new File(dataPath, TitanicConfig.TestFilename)
      val score: Int = (cross.getScore * 10000).asInstanceOf[Int]
      val submitPath = new File(dataPath, "submit-" + now + "_" + score + ".csv")
      val submitInfoPath = new File(dataPath, "submit-" + now + ".txt")
      val pw: PrintWriter = new PrintWriter(new FileWriter(submitInfoPath))
      pw.println("Crossvalidation stats:")
      for(i <- 0 until cross.size) {
        val fold: CrossValidateFold = cross.folds.get(i)
        pw.println("Fold #" + (i + 1) + " : Score: " + fold.score)
      }
      pw.println("Average Score: " + cross.getScore)
      pw.println()
      pw.println(java.util.Arrays.toString(bestNetwork.getLongTermMemory))
      pw.close()
      val fos = new FileOutputStream(submitPath)
      val csv = new CSVWriter(new OutputStreamWriter(fos))
      csv.writeNext(Array[String]("PassengerId", "Survived"))
      val stats = new TitanicStats
      NormalizeTitanic.analyze(stats, trainingPath)
      NormalizeTitanic.analyze(stats, testPath)
      val ids: java.util.List[String] = new java.util.ArrayList[String]
      val training = NormalizeTitanic.normalize(stats, testPath, ids, TitanicConfig.InputNormalizeLow, TitanicConfig.InputNormalizeHigh, TitanicConfig.PredictSurvive, TitanicConfig.PredictPerish)
      var idx: Int = 0
      for (data <- training) {
        val output = bestNetwork.computeRegression(data.input)
        val survived: Int = if (output(0) > 0.5) 1 else 0
        val line: Array[String] = Array(ids.get(idx), "" + survived)
        csv.writeNext(line)
        idx += 1
      }
      csv.close()
      fos.close()
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }
}