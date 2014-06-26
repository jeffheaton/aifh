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
 * Used to calculate survival by male and female.
 */
class CalcSurvival {
  /**
   * Update for a passenger.
   *
   * @param male     True, if passenger was male.
   * @param survived True, if passenger survived.
   */
  def update(male: Boolean, survived: Boolean) {
    if (male)
      countMale += 1
    else
      countFemale += 1

    if (survived) {
      if (male)
        maleSurvive += 1
      else
        femaleSurvive += 1
    }
  }

  /**
   * @return The number of male survivors.
   */
  def getMaleSurvive: Int = maleSurvive

  /**
   * @return The number of female survivors.
   */
  def getFemaleSurvive: Int = femaleSurvive

  /**
   * @return The total count of passengers.
   */
  def getCount: Int = this.countFemale + this.countMale

  /**
   * @return The number of male passengers.
   */
  def getCountMale: Int = countMale

  /**
   * @return The number of female passengers.
   */
  def getCountFemale: Int = countFemale

  override def toString: String = {
    val result: StringBuilder = new StringBuilder
    result.append("(")
    result.append("Count: ")
    result.append(getCount)
    if (getCount > 0) {
      val pct: Double = (this.femaleSurvive + this.maleSurvive).asInstanceOf[Double] / getCount
      result.append(", survived: ")
      result.append(pct)
    }
    if (getCountMale > 0) {
      val pct: Double = maleSurvive.asInstanceOf[Double] / countMale
      result.append(", male.survived: ")
      result.append(pct)
    }
    if (getCountFemale > 0) {
      val pct: Double = femaleSurvive.asInstanceOf[Double] / countFemale
      result.append(", female.survived: ")
      result.append(pct)
    }
    result.append(")")
    result.toString()
  }

  /**
   * The count of males.
   */
  private var countMale: Int = 0
  /**
   * The count of females.
   */
  private var countFemale: Int = 0
  /**
   * The count of male survivors.
   */
  private var maleSurvive: Int = 0
  /**
   * The count of female survivors.
   */
  private var femaleSurvive: Int = 0
}