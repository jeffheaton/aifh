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
package com.heatonresearch.aifh.regression

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.general.fns.link.LogLinkFunction
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test linear reg.
 */
class TestMultipleLinearRegression extends Suite with ShouldMatchers {
  def testBasic() {
    val reg = new MultipleLinearRegression(1)
    assert(2 === reg.longTermMemory.length)

    val lnk = new LogLinkFunction
    reg.linkFunction = lnk
    assert(reg.linkFunction === lnk)
    reg.longTermMemory(0) = 1
    reg.longTermMemory(1) = 2
    val input = Vector(1.0)
    val output = reg.computeRegression(input)
    assert(1 === output.length)
    output(0) should be (1.0986122886681098 plusOrMinus AIFH.DEFAULT_PRECISION)
  }
}