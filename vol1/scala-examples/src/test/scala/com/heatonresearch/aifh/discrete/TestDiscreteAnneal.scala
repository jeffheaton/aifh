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
package com.heatonresearch.aifh.discrete

import com.heatonresearch.aifh.distance.CalculateDistance
import com.heatonresearch.aifh.distance.EuclideanDistance
import com.heatonresearch.aifh.AIFH
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test the discrete anneal subclass.
 */
class TestDiscreteAnneal extends Suite with ShouldMatchers  {
  def testStatus() {
    val anneal = new DiscreteAnnealSubclass(1000, 4000, 1)
    assert("k=0,kMax=1000,t=0.0,prob=0.0" === anneal.getStatus)
  }

  def testGeneral() {
    val anneal = new DiscreteAnnealSubclass(1000, 4000, 1)
    anneal.cycles = 100
    assert(100 === anneal.cycles)
    assert(0 === anneal.getK)
    assert(false === anneal.done)
  }

  def testCoolingSchedule() {
    val anneal = new DiscreteAnnealSubclass(1000, 400, 1)
    anneal.coolingSchedule should be (400.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    anneal.iteration()
    anneal.coolingSchedule should be (397.61057939346017 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testProbability() {
    val anneal = new DiscreteAnnealSubclass(1000, 400, 1)
    anneal.calcProbability(10, 20, anneal.coolingSchedule) should be (0.9753099120283326 plusOrMinus AIFH.DEFAULT_PRECISION)
    anneal.iteration()
    anneal.calcProbability(10, 20, anneal.coolingSchedule) should be (0.9751633961486054 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testRun() {
    val anneal = new DiscreteAnnealSubclass(1000, 400, 1)
    while (!anneal.done) {
      anneal.iteration()
    }
    val dist: CalculateDistance = new EuclideanDistance
    assert(1000 === anneal.getK)
    dist.calculate(anneal.getBest, DiscreteAnnealSubclass.IDEAL) should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    anneal.getBestScore should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }
}