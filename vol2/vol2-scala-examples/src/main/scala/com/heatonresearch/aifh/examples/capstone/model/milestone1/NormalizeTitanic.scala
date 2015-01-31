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
package com.heatonresearch.aifh.examples.capstone.model.milestone1

import au.com.bytecode.opencsv.CSVReader
import au.com.bytecode.opencsv.CSVWriter
import com.heatonresearch.aifh.examples.capstone.model.TitanicConfig
import com.heatonresearch.aifh.examples.util.FormatNumeric
import com.heatonresearch.aifh.general.data.BasicData
import java.io._
import scala.collection.mutable.ListBuffer

/**
 * This capstone project shows how to apply some of the techniques in this book to data science.  The data set used
 * is the Kaggle titanic data set.  You can find that data set here.
 * <p/>
 * http://www.kaggle.com/c/titanic-gettingStarted
 * <p/>
 * There are three parts to this assignment.
 * <p/>
 * Part 1: Obtain and normalize data, extrapolate features
 * Part 2: Cross validate and select model hyperparameters
 * Part 3: Build Kaggle submission file
 * <p/>
 * This is part 1 of the project.
 * <p/>
 * [age,sex-male,pclass,sibsp,parch,fare,embarked-c,embarked-q,embarked-s,name-mil,name-nobility,name-dr,name-clergy]
 */
object NormalizeTitanic {
  /**
   * Analyze and generate stats for titanic data.
   *
   * @param stats    The stats for titanic.
   * @param filename The file to analyze.
   * @return The passenger count.
   * @throws IOException Errors reading file.
   */
  def analyze(stats: TitanicStats, filename: File): Int = {
    var count: Int = 0
    val headerMap: java.util.Map[String, Integer] = new java.util.HashMap[String, Integer]
    val istream: InputStream = new FileInputStream(filename)
    val reader: CSVReader = new CSVReader(new InputStreamReader(istream))
    val header: Array[String] = reader.readNext
    for(i <- 0 until header.length)
      headerMap.put(header(i).toLowerCase, i)

    val ageIndex: Int = headerMap.get("age")
    val nameIndex: Int = headerMap.get("name")
    val sexIndex: Int = headerMap.get("sex")
    val indexEmbarked: Int = headerMap.get("embarked")
    val indexFare: Int = headerMap.get("fare")
    val indexPclass: Int = headerMap.get("pclass")
    var survivedIndex: Int = -1
    if (headerMap.containsKey("survived")) {
      survivedIndex = headerMap.get("survived")
    }
    var nextLine: Array[String] = reader.readNext()
    while (nextLine != null) {
      count += 1
      val name: String = nextLine(nameIndex)
      val ageStr: String = nextLine(ageIndex)
      val sexStr: String = nextLine(sexIndex)
      var embarkedStr: String = nextLine(indexEmbarked)
      var survived: Boolean = false
      if (survivedIndex != -1) {
        val survivedStr: String = nextLine(survivedIndex)
        survived = survivedStr == "1"
      }
      if (indexEmbarked != -1) {
        embarkedStr = nextLine(indexEmbarked)
      }
      val strFare: String = nextLine(indexFare)
      if (strFare.length > 0) {
        val fare: Double = strFare.toDouble
        val pclass: String = nextLine(indexPclass)
        if (pclass == "1") {
          stats.getMeanFare1.update(fare)
        }
        else if (pclass == "2") {
          stats.getMeanFare2.update(fare)
        }
        else if (pclass == "3") {
          stats.getMeanFare3.update(fare)
        }
      }
      val isMale: Boolean = sexStr.equalsIgnoreCase("male")
      var age: Double = .0
      if (survivedIndex != -1) {
        if (embarkedStr == "Q") {
          stats.getEmbarkedQ.update(isMale, survived)
        }
        else if (embarkedStr == "S") {
          stats.getEmbarkedS.update(isMale, survived)
        }
        else if (embarkedStr == "C") {
          stats.getEmbarkedC.update(isMale, survived)
        }
      }
      stats.getEmbarkedHisto.update(embarkedStr)
      if (survivedIndex != -1) {
        stats.getSurvivalTotal.update(isMale, survived)
      }
      if (survivedIndex != -1) {
        if (name.contains("Master.")) {
          stats.getSurvivalMaster.update(isMale, survived)
        }
        else if (name.contains("Mr.")) {
          stats.getSurvivalMr.update(isMale, survived)
        }
        else if (name.contains("Miss.") || name.contains("Mlle.")) {
          stats.getSurvivalMiss.update(isMale, survived)
        }
        else if (name.contains("Mrs.") || name.contains("Mme.")) {
          stats.getSurvivalMrs.update(isMale, survived)
        }
        else if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) {
          stats.getSurvivalMilitary.update(isMale, survived)
        }
        else if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) {
          stats.getSurvivalNobility.update(isMale, survived)
        }
        else if (name.contains("Dr.")) {
          stats.getSurvivalDr.update(isMale, survived)
        }
        else if (name.contains("Rev.")) {
          stats.getSurvivalClergy.update(isMale, survived)
        }
      }
      if (ageStr.length > 0) {
        age = ageStr.toDouble
        if (isMale) {
          stats.getMeanMale.update(age)
        }
        else {
          stats.getMeanFemale.update(age)
        }
        stats.getMeanTotal.update(age)
        if (name.contains("Master.")) {
          stats.getMeanMaster.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalMaster.update(isMale, survived)
          }
        }
        else if (name.contains("Mr.")) {
          stats.getMeanMr.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalMr.update(isMale, survived)
          }
        }
        else if (name.contains("Miss.") || name.contains("Mlle.")) {
          stats.getMeanMiss.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalMiss.update(isMale, survived)
          }
        }
        else if (name.contains("Mrs.") || name.contains("Mme.")) {
          stats.getMeanMrs.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalMrs.update(isMale, survived)
          }
        }
        else if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) {
          stats.getMeanMilitary.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalMilitary.update(isMale, survived)
          }
        }
        else if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) {
          stats.getMeanNobility.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalNobility.update(isMale, survived)
          }
        }
        else if (name.contains("Dr.")) {
          stats.getMeanDr.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalDr.update(isMale, survived)
          }
        }
        else if (name.contains("Rev.")) {
          stats.getMeanClergy.update(age)
          if (survivedIndex != -1) {
            stats.getSurvivalClergy.update(isMale, survived)
          }
        }
      }
      nextLine = reader.readNext()
    }
    count
  }

  /**
   * Normalize to a range.
   *
   * @param x              The value to normalize.
   * @param dataLow        The low end of the range of the data.
   * @param dataHigh       The high end of the range of the data.
   * @param normalizedLow  The normalized low end of the range of data.
   * @param normalizedHigh The normalized high end of the range of data.
   * @return The normalized value.
   */
  def rangeNormalize(x: Double, dataLow: Double, dataHigh: Double, normalizedLow: Double, normalizedHigh: Double): Double = {
    ((x - dataLow) / (dataHigh - dataLow)) * (normalizedHigh - normalizedLow) + normalizedLow
  }

  def normalize(stats: TitanicStats, filename: File, ids: java.util.List[String], inputLow: Double, inputHigh: Double,
                predictSurvive: Double, predictPerish: Double): List[BasicData] = {
    val result = ListBuffer[BasicData]()
    val headerMap: java.util.Map[String, Integer] = new java.util.HashMap[String, Integer]
    val iStream: InputStream = new FileInputStream(filename)
    val reader = new CSVReader(new InputStreamReader(iStream))
    val header: Array[String] = reader.readNext
    for(i <- 0 until header.length) {
      headerMap.put(header(i).toLowerCase, i)
    }
    val ageIndex: Int = headerMap.get("age")
    val nameIndex: Int = headerMap.get("name")
    val sexIndex: Int = headerMap.get("sex")
    val indexEmbarked: Int = headerMap.get("embarked")
    val indexPclass: Int = headerMap.get("pclass")
    val indexSibsp: Int = headerMap.get("sibsp")
    val indexParch: Int = headerMap.get("parch")
    val indexFare: Int = headerMap.get("fare")
    val indexId: Int = headerMap.get("passengerid")
    var survivedIndex: Int = -1
    if (headerMap.containsKey("survived")) {
      survivedIndex = headerMap.get("survived")
    }
    var nextLine: Array[String] = reader.readNext
    while (nextLine != null) {
      val data: BasicData = new BasicData(TitanicConfig.InputFeatureCount, 1)
      val name = nextLine(nameIndex)
      val sex = nextLine(sexIndex)
      val embarked = nextLine(indexEmbarked)
      val id = nextLine(indexId)
      if (ids != null) {
        ids.add(id)
      }
      val isMale = sex.equalsIgnoreCase("male")
      var age = 0.0
      if (nextLine(ageIndex).length == 0) {
        if (name.contains("Master.")) {
          age = stats.getMeanMaster.calculate
        }
        else if (name.contains("Mr.")) {
          age = stats.getMeanMr.calculate
        }
        else if (name.contains("Miss.") || name.contains("Mlle.")) {
          age = stats.getMeanMiss.calculate
        }
        else if (name.contains("Mrs.") || name.contains("Mme.")) {
          age = stats.getMeanMrs.calculate
        }
        else if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) {
          age = stats.getMeanMiss.calculate
        }
        else if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) {
          age = stats.getMeanNobility.calculate
        }
        else if (name.contains("Dr.")) {
          age = stats.getMeanDr.calculate
        }
        else if (name.contains("Rev.")) {
          age = stats.getMeanClergy.calculate
        }
        else {
          if (isMale) {
            age = stats.getMeanMale.calculate
          }
          else {
            age = stats.getMeanFemale.calculate
          }
        }
      }
      else {
        age = nextLine(ageIndex).toDouble
      }
      data.input(0) = rangeNormalize(age, 0, 100, inputLow, inputHigh)
      data.input(1) = if (isMale) inputHigh else inputLow
      val pclass = nextLine(indexPclass).toDouble
      data.input(2) = rangeNormalize(pclass, 1, 3, inputLow, inputHigh)
      val sibsp = nextLine(indexSibsp).toDouble
      data.input(3) = rangeNormalize(sibsp, 0, 10, inputLow, inputHigh)
      val parch = nextLine(indexParch).toDouble
      data.input(4) = rangeNormalize(parch, 0, 10, inputLow, inputHigh)
      val strFare: String = nextLine(indexFare)
      var fare: Double = .0
      if (strFare.length == 0) {
        if (pclass.asInstanceOf[Int] == 1) {
          fare = stats.getMeanFare1.calculate
        }
        else if (pclass.asInstanceOf[Int] == 2) {
          fare = stats.getMeanFare2.calculate
        }
        else if (pclass.asInstanceOf[Int] == 3) {
          fare = stats.getMeanFare3.calculate
        }
        else {
          fare = stats.getMeanFare2.calculate
        }
      }
      else {
        fare = nextLine(indexFare).toDouble
      }
      data.input(5) = rangeNormalize(fare, 0, 500, inputLow, inputHigh)
      data.input(6) = if (embarked.trim.equalsIgnoreCase("c")) inputHigh else inputLow
      data.input(7) = if (embarked.trim.equalsIgnoreCase("q")) inputHigh else inputLow
      data.input(8) = if (embarked.trim.equalsIgnoreCase("s")) inputHigh else inputLow
      data.input(9) = if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) inputHigh else inputLow
      data.input(10) = if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) inputHigh else inputLow
      data.input(11) = if (name.contains("Dr.")) inputHigh else inputLow
      data.input(12) = if (name.contains("Rev.")) inputHigh else inputLow
      result += data
      if (survivedIndex != -1) {
        val survived = nextLine(survivedIndex).toInt
        data.ideal(0) = if (survived == 1) predictSurvive else predictPerish
      }
      nextLine = reader.readNext()
    }
    result.toList
  }

  /**
   * The main method.
   *
   * @param args The arguments.
   */
  def main(args: Array[String]) {
    var filename: String = null
    if (args.length != 1) {
      filename = System.getProperty("FILENAME")
      if (filename == null) {
        println("Please call this program with a single parameter that specifies your data directory.\n" + "If you are calling with gradle, consider:\n" + "gradle runCapstoneTitanic1 -Pdata_path=[path to your data directory]\n")
        System.exit(0)
      }
    }
    else {
      filename = args(0)
    }
    val dataPath = new File(filename)
    val trainingPath = new File(dataPath, TitanicConfig.TrainingFilename)
    val testPath = new File(dataPath, TitanicConfig.TestFilename)
    val normalizePath = new File(dataPath, TitanicConfig.NormDumpFilename)
    try {
      val stats = new TitanicStats
      analyze(stats, trainingPath)
      analyze(stats, testPath)
      stats.dump()
      val ids: java.util.List[String] = new java.util.ArrayList[String]
      val training = normalize(stats, trainingPath, ids, TitanicConfig.InputNormalizeLow, TitanicConfig.InputNormalizeHigh, TitanicConfig.PredictSurvive, TitanicConfig.PredictPerish)
      val fos = new FileOutputStream(normalizePath)
      val csv = new CSVWriter(new OutputStreamWriter(fos))
      csv.writeNext(Array[String]("id", "age", "sex-male", "pclass", "sibsp", "parch", "fare", "embarked-c", "embarked-q", "embarked-s", "name-mil", "name-nobility", "name-dr", "name-clergy"))
      var idx: Int = 0
      for (data <- training) {
        val line = Array(ids.get(idx),
          FormatNumeric.formatDouble(data.input(0), 5),
          FormatNumeric.formatDouble(data.input(1), 5),
          FormatNumeric.formatDouble(data.input(2), 5),
          FormatNumeric.formatDouble(data.input(3), 5),
          FormatNumeric.formatDouble(data.input(4), 5),
          FormatNumeric.formatDouble(data.input(5), 5),
          FormatNumeric.formatDouble(data.input(6), 5),
          FormatNumeric.formatDouble(data.input(7), 5),
          FormatNumeric.formatDouble(data.input(8), 5),
          FormatNumeric.formatDouble(data.input(9), 5),
          FormatNumeric.formatDouble(data.input(10), 5),
          FormatNumeric.formatDouble(data.input(11), 5),
          FormatNumeric.formatDouble(data.input(12), 5),
          FormatNumeric.formatDouble(data.ideal(0), 5))
        idx += 1
        csv.writeNext(line)
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