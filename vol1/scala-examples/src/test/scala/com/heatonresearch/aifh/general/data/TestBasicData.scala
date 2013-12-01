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
package com.heatonresearch.aifh.general.data

import com.heatonresearch.aifh.AIFH
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test basic data.
 */
object TestBasicData {
  val TEST_INPUT = Vector(Vector(0.0, 0.0), Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0))
  val TEST_IDEAL = Vector(Vector(0.0), Vector(1.0), Vector(1.0), Vector(0.0))
}

class TestBasicData extends Suite with ShouldMatchers {
  def testUnSupervised() {
    val data = new BasicData(2)
    assert(2 === data.input.length)
    assert(0 === data.ideal.length)
  }

  def testSupervised() {
    val data = new BasicData(2, 1)
    assert(2 === data.input.length)
    assert(1 === data.ideal.length)
  }

  def testToString() {
    val data = new BasicData(2)
    assert("[BasicData: input:Vector(0.0, 0.0), ideal:Vector(), label:null]" === data.toString)
  }

  def testBasicData() {
    val a = Vector(1.0, 2.0)
    val d = new BasicData(a)
    assert(2 === d.input.length)
    assert(0 === d.ideal.length)
    d.input(0) should be (1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    d.input(1) should be (2.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testArrays() {
    val list = BasicData.convertArrays(TestBasicData.TEST_INPUT, TestBasicData.TEST_IDEAL)
    assert(4 === list.size)
    assert(2 === list(0).input.length)
    assert(1 === list(0).ideal.length)
    list(1).input(0) should be (1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    list(1).input(1) should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    list(1).ideal(0) should be (1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }
}