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
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test training.
 */
class TestTraining extends Suite with ShouldMatchers {
  private def performTest(train: LearningMethod) {
    assert(!train.done)
    train.getStatus
    train.iteration()
    val startError: Double = train.getLastError
    var i: Int = 0
    while (i < 1000 && !train.done) {
      train.iteration()
      i+= 1
    }
    train.iteration()
    train.finishTraining()
    assert((train.getLastError < startError) || Math.abs(train.getLastError) < 1)
  }

  def testAnneal() {
    val anneal = new TrainAnneal(new TrialAlgo, new TrialScore)
    performTest(anneal)
  }

  def testGreedyRandom() {
    val train: TrainGreedyRandom = new TrainGreedyRandom(true, new TrialAlgo, new TrialScore)
    train.lowRange = 0
    train.highRange = 10
    train.lowRange should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    train.highRange should be (10.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    performTest(train)
  }

  def testHillClimbing() {
    val train = new TrainHillClimb(theShouldMinimize = true, new TrialAlgo, new TrialScore)
    performTest(train)
  }

  def testNelderMead() {
    val train = new TrainNelderMead(new TrialAlgo, new TrialScore)
    performTest(train)
  }
}