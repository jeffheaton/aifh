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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.AIFH
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

/**
 * Test anneal learning.
 */
class TestLearnAnneal extends Suite with ShouldMatchers {
  def testBasic() {
    val anneal = new TrainAnneal(new TrialAlgo, new TrialScore)
    anneal.coolingSchedule should be (400.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testGetStatus() {
    val anneal = new TrainAnneal(new TrialAlgo, new TrialScore)
    assert("k=0,kMax=1000,t=0.0,prob=0.0" === anneal.getStatus)
  }

  def testRandomize() {
    val algo = new TrialAlgo
    val anneal = new TrainAnneal(algo, new TrialScore)
    anneal.performRandomize(algo.longTermMemory)
    anneal.finishTraining()
    algo.longTermMemory(0) should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testIterations() {
    val anneal = new TrainAnneal(new TrialAlgo, new TrialScore, 10, 400, 0.0001)
    anneal.cycles = 10
    anneal.coolingSchedule should be (400.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    anneal.startingTemperature should be (400.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    anneal.endingTemperature should be (0.0001 plusOrMinus  AIFH.DEFAULT_PRECISION)
    assert(10 === anneal.cycles)
    anneal.getCurrentTemperature should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    assert(0 === anneal.getK)
    assert(false === anneal.done)
    assert(true === anneal.getLastError.isInfinite)
    anneal.getLastProbability should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    anneal.iteration()
    assert(anneal.getLastError > 0)
    anneal.getCurrentTemperature should be (87.46896591546223 plusOrMinus AIFH.DEFAULT_PRECISION)
    assert(1 === anneal.getK)
    assert(!anneal.done)
    for(i <- 0 until 9)
      anneal.iteration()

    assert(anneal.done)
    anneal.getCurrentTemperature should be (9.999999999999E-5 plusOrMinus AIFH.DEFAULT_PRECISION)
    assert(10 === anneal.getK)
  }
}