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
package com.heatonresearch.aifh.normalize

import com.heatonresearch.aifh.AIFH
import java.io.File
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test the data set.
 */
class TestDataSet extends Suite with ShouldMatchers {
  private def generateTestData: DataSet = {
    val headers = Array("text", "numeric", "dec")
    val ds: DataSet = new DataSet(headers)
    ds.add(Array[AnyRef]("One", "1", "0.1"))
    ds.add(Array[AnyRef]("Two", "2", "0.2"))
    ds.add(Array[AnyRef]("Three", "3", "0.3"))
    ds
  }

  def testLoadSave() {
    val ds: DataSet = generateTestData
    val filename: File = new File("deleteme.csv")
    DataSet.save(filename, ds)
    val dataset2 = DataSet.load(filename)
    assert(filename.delete)
    assert(ds == dataset2)
    assert(dataset2 == ds)
    assert(3 === ds.size)
    assert(3 === ds.getHeaderCount)
  }

  def testEqual() {
    val ds1 = generateTestData
    val ds2 = generateTestData
    assert(ds1 === ds2)
  }

  def testNotEqualHeaders() {
    val ds1 = generateTestData
    val ds2 = generateTestData
    ds1.getHeaders(1) = "--"
    assert(ds1 != ds2)
  }

  def testNotEqualHeaderCount() {
    val ds1 = generateTestData
    val ds2 = generateTestData
    ds1.appendColumns(1)
    assert(ds1 != ds2)
  }

//  def testNotEqualRowCount() {
//    val ds1 = generateTestData
//    val ds2 = generateTestData
//    ds1.getData.remove(0)
//    assert(ds1 != ds2)
//  }

  def testNotEqualRows() {
    val ds1 = generateTestData
    val ds2 = generateTestData
    ds1.getData(0)(0) = "---"
    assert(ds1 != ds2)
  }

  def testNotEqualOtherObject() {
    val ds1 = generateTestData
    assert(ds1 != "")
  }

  def testMin() {
    val ds1 = generateTestData
    ds1.getMin(1) should be (1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    // test again, as strings are now numbers, from the last call
    ds1.getMin(1) should be (1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testMax() {
    val ds1 = generateTestData
    ds1.getMax(1) should be (3.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    // test again, as strings are now numbers, from the last call
    ds1.getMax(1) should be (3.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testNormalizeRange() {
    val ds1 = generateTestData
    ds1.normalizeRange(1, -1, 1)
    ds1.getData(0)(1).toString.toDouble should be (-1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testDeNormalizeRange() {
    val ds1 = generateTestData
    val min = ds1.getMin(2)
    val max = ds1.getMax(2)
    ds1.normalizeRange(2, min, max, -1, 1)
    ds1.getData(0)(2).toString.toDouble should be (-1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    ds1.deNormalizeRange(2, min, max, -1, 1)
    ds1.getData(0)(2).toString.toDouble should be (0.1 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testNormalizeReciprocal() {
    val ds1 = generateTestData
    ds1.normalizeReciprocal(1)
    ds1.getData(1)(1).toString.toDouble should be (0.5 plusOrMinus AIFH.DEFAULT_PRECISION)
    ds1.deNormalizeReciprocal(1)
    ds1.getData(1)(1).toString.toDouble should be (2.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testEncodeNumeric() {
    val ds1 = generateTestData
    ds1.encodeNumeric(0)
  }

  def testEncodeOneOfN() {
    val ds1 = generateTestData
    ds1.encodeOneOfN(0)
  }

  def testEncodeEquilateral() {
    val ds1 = generateTestData
    ds1.encodeEquilateral(0)
  }

  def testDeleteColumn() {
    val ds1 = generateTestData
    ds1.deleteColumn(0)
    assert(2 === ds1.getHeaderCount)
    assert(ds1.getHeaders(0) === "numeric")
    assert(ds1.getHeaders(1) === "dec")
  }

  def testExtractUnsupervisedLabeled() {
    val ds1 = generateTestData
    val result = ds1.extractUnsupervisedLabeled(0)
    assert(3 === result.size)
    assert(result(0).label === "One")
  }

  def testExtractSupervised() {
    val ds1: DataSet = generateTestData
    val result = ds1.extractSupervised(1, 1, 2, 1)
    assert(3 === result.size)
  }

  def testReplaceColumn() {
    val ds1 = generateTestData
    ds1.replaceColumn(1, 2, 1, 0)
    val result = ds1.extractSupervised(1, 1, 2, 1)
    result(0).input(0) should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    result(1).input(0) should be (1.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testDeleteUnknowns() {
    val ds1 = generateTestData
    ds1.getData(1)(2) = "?"
    ds1.deleteUnknowns()
    assert(2 === ds1.getData.size)
  }
}