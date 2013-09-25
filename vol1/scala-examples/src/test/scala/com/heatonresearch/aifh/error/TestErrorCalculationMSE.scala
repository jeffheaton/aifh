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
package com.heatonresearch.aifh.error

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

/**
 * Test MSE error calc.
 */
class TestErrorCalculationMSE extends Suite with ShouldMatchers {
  def testErrorCalc() {
    val calc = new ErrorCalculationMSE
    val result = ErrorTestingUtil.calculateError(calc, ErrorTestingUtil.ACTUAL, ErrorTestingUtil.IDEAL)
    result should be (151.6205 plusOrMinus 0.001)
  }
}