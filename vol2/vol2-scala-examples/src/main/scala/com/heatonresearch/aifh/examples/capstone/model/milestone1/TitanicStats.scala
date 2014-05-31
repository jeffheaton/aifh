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

/**
 * Stats that were collected about the titanic passengers to help with normalization, interpolation
 * and modeling.
 */
class TitanicStats {
  /**
   * @return Passengers with the title "master", mean age.
   */
  def getMeanMaster: CalcMean = meanMaster

  /**
   * @return Passengers with the title "mr", mean age.
   */
  def getMeanMr: CalcMean = meanMr

  /**
   * @return Passengers with the title "miss", mean age.
   */
  def getMeanMiss: CalcMean = meanMiss

  /**
   * @return Passengers with the title "mrs", mean age.
   */
  def getMeanMrs: CalcMean = meanMrs

  /**
   * @return Passengers with a military title, mean age.
   */
  def getMeanMilitary: CalcMean = meanMilitary

  /**
   * @return Passengers with a noble title, mean age.
   */
  def getMeanNobility: CalcMean = meanNobility

  /**
   * @return Passengers with the title "dr", mean age.
   */
  def getMeanDr: CalcMean = meanDr

  /**
   * @return Passengers with the title "rev", mean age.
   */
  def getMeanClergy: CalcMean = meanClergy

  /**
   * @return Mean age for all passengers.
   */
  def getMeanTotal: CalcMean = meanTotal

  /**
   * @return Survival stats for passengers with a title of "master".
   */
  def getSurvivalMaster: CalcSurvival = survivalMaster

  /**
   * @return Survival stats for passengers with a title of "mr".
   */
  def getSurvivalMr: CalcSurvival = survivalMr

  /**
   * @return Survival stats for passengers with a title of "miss".
   */
  def getSurvivalMiss: CalcSurvival = survivalMiss

  /**
   * @return Survival stats for passengers with a title of "mrs".
   */
  def getSurvivalMrs: CalcSurvival = survivalMrs

  /**
   * @return Survival stats for passengers with a military title.
   */
  def getSurvivalMilitary: CalcSurvival = survivalMilitary

  /**
   * @return Survival stats for passengers with a noble title.
   */
  def getSurvivalNobility: CalcSurvival = survivalNobility

  /**
   * @return Survival stats for passengers with a title of "dr".
   */
  def getSurvivalDr: CalcSurvival = survivalDr

  /**
   * @return Survival stats for passengers with a title of "clergy".
   */
  def getSurvivalClergy: CalcSurvival = survivalClergy

  /**
   * @return Survival stats on the total number of passengers.
   */
  def getSurvivalTotal: CalcSurvival = survivalTotal

  /**
   * @return Survival stats for passengers that embarked from Southampton, England.
   */
  def getEmbarkedS: CalcSurvival = embarkedS

  /**
   * @return Survival stats for passengers that embarked from Cherbourg, France.
   */
  def getEmbarkedC: CalcSurvival = embarkedC

  /**
   * @return Survival stats for passengers that embarked from Queenstown, England.
   */
  def getEmbarkedQ: CalcSurvival = embarkedQ

  /**
   * @return Histogram of embark locations.
   */
  def getEmbarkedHisto: CalcHistogram = embarkedHisto

  /**
   * @return Mean age for male passengers.
   */
  def getMeanMale: CalcMean = meanMale

  /**
   * @return Mean age for female passengers.
   */
  def getMeanFemale: CalcMean = meanFemale

  /**
   * @return Mean fare for first class.
   */
  def getMeanFare1: CalcMean = meanFare1

  /**
   * @return Mean fare for second class.
   */
  def getMeanFare2: CalcMean = meanFare2

  /**
   * @return Mean fare for second class.
   */
  def getMeanFare3: CalcMean = meanFare3

  /**
   * Dump all stats to stdout.
   */
  def dump() {
    println("Mean Master: Mean Age: " + meanMaster.calculate + " " + survivalMaster.toString)
    println("Mr.: Mean Age: " + meanMr.calculate + " " + survivalMr.toString)
    println("Miss.: Mean Age: " + meanMiss.calculate + " " + survivalMiss.toString)
    println("Mrs.: Mean Age: " + meanMrs.calculate + " " + survivalMrs.toString)
    println("Military: Mean Age: " + meanMrs.calculate + " " + survivalMilitary.toString)
    println("Clergy: Mean Age: " + meanClergy.calculate + " " + survivalClergy.toString)
    println("Nobility: Mean Age: " + meanNobility.calculate + " " + survivalNobility.toString)
    println("Dr: Mean Age: " + meanDr.calculate + " " + survivalDr.toString)
    println("Total known survival: Mean Age: " + meanTotal.calculate + " " + survivalTotal.toString)
    println()
    println("Embarked Queenstown: Mean Age: " + embarkedQ.toString)
    println("Embarked Southampton: Mean Age: " + embarkedS.toString)
    println("Embarked Cherbourg: Mean Age: " + embarkedC.toString)
    println("Most common embarked: Mean Age: " + this.embarkedHisto.max)
    println()
    println("Mean Age Male: " + this.meanMale.calculate)
    println("Mean Age Female: " + this.meanFemale.calculate)
    println()
    println("Mean Fair 1st Class: " + this.meanFare1.calculate)
    println("Mean Fair 2st Class: " + this.meanFare2.calculate)
    println("Mean Fair 3st Class: " + this.meanFare3.calculate)
  }

  /**
   * Passengers with the title "master", mean age.
   */
  private val meanMaster: CalcMean = new CalcMean
  /**
   * Passengers with the title "mr", mean age.
   */
  private val meanMr: CalcMean = new CalcMean
  /**
   * Passengers with the title "miss", mean age.
   */
  private val meanMiss: CalcMean = new CalcMean
  /**
   * Passengers with the title "mrs", mean age.
   */
  private val meanMrs: CalcMean = new CalcMean
  /**
   * Passengers with a military title, mean age.
   */
  private val meanMilitary: CalcMean = new CalcMean
  /**
   * Passengers with a nobility title, mean age.
   */
  private val meanNobility: CalcMean = new CalcMean
  /**
   * Passengers with the title "dr".
   */
  private val meanDr: CalcMean = new CalcMean
  /**
   * Passengers with the title "rev".
   */
  private val meanClergy: CalcMean = new CalcMean
  /**
   * Total passengers.
   */
  private val meanTotal: CalcMean = new CalcMean
  /**
   * Total male passengers.
   */
  private val meanMale: CalcMean = new CalcMean
  /**
   * Total female passengers.
   */
  private val meanFemale: CalcMean = new CalcMean
  /**
   * Passengers in 1st class, average fare.
   */
  private val meanFare1: CalcMean = new CalcMean
  /**
   * Passengers in 2st class, average fare.
   */
  private val meanFare2: CalcMean = new CalcMean
  /**
   * Passengers in 3rd class, average fare.
   */
  private val meanFare3: CalcMean = new CalcMean
  /**
   * Survival stats for passengers with a title of "master".
   */
  private val survivalMaster: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a title of "mr".
   */
  private val survivalMr: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a title of "miss".
   */
  private val survivalMiss: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a title of "mrs".
   */
  private val survivalMrs: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a military title.
   */
  private val survivalMilitary: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a nobility title.
   */
  private val survivalNobility: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a title of "dr".
   */
  private val survivalDr: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers with a title of "rev".
   */
  private val survivalClergy: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for all passengers.
   */
  private val survivalTotal: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers that embarked from Southampton, England.
   */
  private val embarkedS: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers that embarked from Cherbourg, France.
   */
  private val embarkedC: CalcSurvival = new CalcSurvival
  /**
   * Survival stats for passengers that embarked from Queenstown, England.
   */
  private val embarkedQ: CalcSurvival = new CalcSurvival
  /**
   * Histogram of embark locations.
   */
  private val embarkedHisto: CalcHistogram = new CalcHistogram
}