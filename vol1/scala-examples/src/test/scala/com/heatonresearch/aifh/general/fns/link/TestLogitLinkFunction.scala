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
package com.heatonresearch.aifh.general.fns.link

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.AIFHError
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test the Logit link function.
 */
class TestLogitLinkFunction extends Suite with ShouldMatchers {
  def testEvaluate() {
    val fn = new LogitLinkFunction
    val x = Vector(2.0)
    val y = fn.evaluate(x)
    y should be (0.8807970779778823 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testException() {
    val fn = new LogitLinkFunction
    val x = Vector(1.0, 2.0)
    intercept[AIFHError] {
      fn.evaluate(x)
    }
  }
}