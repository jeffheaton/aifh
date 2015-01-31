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
package com.heatonresearch.aifh.normalize

import au.com.bytecode.opencsv.CSVReader
import au.com.bytecode.opencsv.CSVWriter
import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.general.data.BasicData
import java.io._
import java.text.NumberFormat
import java.text.ParseException
import java.util._

/**
 * Holds a data set.  This is usually loaded from a CSV.  It can also be generated.
 */
object DataSet {
  /**
   * Load a CSV file from a file.
   *
   * @param filename The filename.
   * @return The data set read.
   */
  def load(filename: File): DataSet = {
    try {
      val fis: FileInputStream = new FileInputStream(filename)
      val ds: DataSet = load(fis)
      fis.close()
      ds
    }
    catch {
      case ex: IOException =>
        throw new AIFHError(ex)
    }
  }

  /**
   * Load a CSV from an input stream.
   *
   * @param is The input stream.
   * @return The loaded file.
   */
  def load(is: InputStream): DataSet = {
    try {
      val reader: Reader = new InputStreamReader(is)
      val csv: CSVReader = new CSVReader(reader)
      val headers: Array[String] = csv.readNext
      val result: DataSet = new DataSet(headers)
      var nextLine: Array[String] = csv.readNext()
      class ContinueException extends Exception
      try {
        while (nextLine != null) {
          if (nextLine.length <= 1) {
            throw new ContinueException
          }
          else if (nextLine.length != result.getHeaderCount) {
            throw new AIFHError("Found a CSV line with " + nextLine.length + " columns, when expecting " + result.getHeaderCount)
          }
          val obj: Array[AnyRef] = new Array[AnyRef](result.getHeaderCount)
          System.arraycopy(nextLine, 0, obj, 0, nextLine.length)
          result.add(obj)
          nextLine = csv.readNext()
        }
      } catch {
        case _ : ContinueException =>
      }
      csv.close()
      result
    }
    catch {
      case ex: IOException =>
        throw new AIFHError(ex)
    }
  }

  /**
   * Save the specified data set to a CSV file.
   *
   * @param filename The filename.
   * @param ds       The data set to save.
   */
  def save(filename: File, ds: DataSet) {
    try {
      val fos: FileOutputStream = new FileOutputStream(filename)
      save(fos, ds)
      fos.close()
    }
    catch {
      case ex: IOException =>
        throw new AIFHError(ex)
    }
  }

  /**
   * Save the specified data to an output stream.
   *
   * @param os The output stream.
   * @param ds The data set.
   */
  def save(os: OutputStream, ds: DataSet) {
    try {
      val writer: Writer = new OutputStreamWriter(os)
      val csv: CSVWriter = new CSVWriter(writer)
      csv.writeNext(ds.getHeaders)
      val items2: Array[String] = new Array[String](ds.getHeaderCount)
      import scala.collection.JavaConversions._
      for (item <- ds.getData) {
        for(i <- 0 until ds.getHeaderCount)
          items2(i) = item(i).toString
        csv.writeNext(items2)
      }
      csv.close()
    }
    catch {
      case ex: IOException =>
        throw new AIFHError(ex)
    }
  }
}

class DataSet {
  /**
   * Create a blank data set.
   *
   * @param theHeaders The column headers.
   */
  def this(theHeaders: Array[String]) {
    this()
    this.headers = theHeaders
  }

  /**
   * Convert a column to numeric.  Save the new Double object in place of the string.
   *
   * @param obj    The column array.
   * @param column The column to change.
   * @return The numeric value.
   */
  private def convertNumeric(obj: Array[AnyRef], column: Int): Double = {
    var x: Double = .0
    obj(column) match {
      case x1: java.lang.Double =>
        x = x1
      case _ =>
        try {
          x = this.numberFormatter.parse(obj(column).toString).doubleValue
          obj(column) = x.asInstanceOf[AnyRef]
        }
        catch {
          case e: ParseException =>
            throw new AIFHError(e)
        }
    }
    x
  }

  /**
   * @return The number of columns (or headers).
   */
  def getHeaderCount: Int = headers.length

  /**
   * @return The column headers.
   */
  def getHeaders: Array[String] = headers

  /**
   * Add a row.
   *
   * @param row The row to add.
   */
  def add(row: Array[AnyRef]) {
    this.data.add(row)
  }

  /**
   * @return The row data.
   */
  def getData: java.util.List[Array[AnyRef]] = this.data

  /**
   * Get the maximum numeric value for a column.
   *
   * @param column The column.
   * @return The max numeric value.
   */
  def getMax(column: Int): Double = {
    var result: Double = Double.NegativeInfinity
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      result = Math.max(result, convertNumeric(obj, column))
    }
    result
  }

  /**
   * Get the minimum numeric value for a column.
   *
   * @param column The column.
   * @return The min numeric value.
   */
  def getMin(column: Int): Double = {
    var result: Double = Double.PositiveInfinity
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      result = Math.min(result, convertNumeric(obj, column))
    }
    result
  }

  /**
   * Normalize a column using range normalization.
   * http://www.heatonresearch.com/wiki/Range_Normalization
   *
   * @param column         The column to normalize.
   * @param dataLow        The low value for the actual data.
   * @param dataHigh       The high value for the actual data.
   * @param normalizedLow  The desired low normalized value.
   * @param normalizedHigh The desired high normalized value.
   */
  def normalizeRange(column: Int, dataLow: Double, dataHigh: Double, normalizedLow: Double, normalizedHigh: Double) {
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      val x: Double = convertNumeric(obj, column)
      obj(column) = (((x - dataLow) / (dataHigh - dataLow)) * (normalizedHigh - normalizedLow) + normalizedLow).asInstanceOf[AnyRef]
    }
  }

  /**
   * Normalize a column using range normalization.  Automatically determine the actual data high and low.
   * http://www.heatonresearch.com/wiki/Range_Normalization
   *
   * @param column         The column to normalize.
   * @param normalizedLow  The desired low normalized value.
   * @param normalizedHigh The desired high normalized value.
   */
  def normalizeRange(column: Int, normalizedLow: Double, normalizedHigh: Double) {
    val dataLow: Double = getMin(column)
    val dataHigh: Double = getMax(column)
    normalizeRange(column, dataLow, dataHigh, normalizedLow, normalizedHigh)
  }

  /**
   * De-Normalize a column using range normalization.
   * http://www.heatonresearch.com/wiki/Range_Normalization
   *
   * @param column         The column to normalize.
   * @param dataLow        The low value for the actual data.
   * @param dataHigh       The high value for the actual data.
   * @param normalizedLow  The desired low normalized value.
   * @param normalizedHigh The desired high normalized value.
   */
  def deNormalizeRange(column: Int, dataLow: Double, dataHigh: Double, normalizedLow: Double, normalizedHigh: Double) {
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      val x: Double = convertNumeric(obj, column)
      obj(column) = (((dataLow - dataHigh) * x - normalizedHigh * dataLow + dataHigh * normalizedLow) / (normalizedLow - normalizedHigh)).asInstanceOf[AnyRef]
    }
  }

  /**
   * Normalize a column using reciprocal normalization.
   * http://www.heatonresearch.com/wiki/Reciprocal_Normalization
   *
   * @param column The column to encode.
   */
  def normalizeReciprocal(column: Int) {
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      val x: Double = convertNumeric(obj, column)
      obj(column) = (1 / x).asInstanceOf[AnyRef]
    }
  }

  /**
   * De-Normalize a column using reciprocal normalization.
   * Note: normalization and de-normalization are the same mathematical operation.
   * http://www.heatonresearch.com/wiki/Reciprocal_Normalization
   *
   * @param column The column to encode.
   */
  def deNormalizeReciprocal(column: Int) {
    normalizeReciprocal(column)
  }

  /**
   * Enumerate classes (factors) into a numbered set.
   *
   * @param column The column to enumerate.
   * @return The numbered set.
   */
  def enumerateClasses(column: Int): java.util.Map[String, Integer] = {
    val classes: java.util.Set[String] = new java.util.HashSet[String]
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      classes.add(obj(column).toString)
    }
    val result = new java.util.HashMap[String, Integer]
    var index: Int = 0
    import scala.collection.JavaConversions._
    for (className <- classes) {
      result.put(className,index)
      index += 1
    }
    result
  }

  /**
   * Encode (enumerate) a column with simple numeric index encoding.
   *
   * @param column The column to encode.
   * @return The mapping from column names to indexes.
   */
  def encodeNumeric(column: Int): java.util.Map[String, Integer] = {
    val classes: java.util.Map[String, Integer] = enumerateClasses(column)
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      val index: Int = classes.get(obj(column).toString)
      obj(column) = index.asInstanceOf[AnyRef]
    }
    classes
  }

  /**
   * Encode a column using "one of n" encoding.  Use 0 for the off value, and 1 for on.
   * <p/>
   * http://www.heatonresearch.com/wiki/One_of_n
   *
   * @param column The column to use.
   * @return The column to index mapping (the same result as calling enumerateClasses).
   */
  def encodeOneOfN(column: Int): java.util.Map[String, Integer] = encodeOneOfN(column, 0, 1)

  /**
   * Encode a column using "one of n" encoding.
   * <p/>
   * http://www.heatonresearch.com/wiki/One_of_n
   *
   * @param column   The column to use.
   * @param offValue The off value to use.
   * @param onValue  The on value to use.
   * @return The column to index mapping (the same result as calling enumerateClasses).
   */
  def encodeOneOfN(column: Int, offValue: Double, onValue: Double): java.util.Map[String, Integer] = {
    val name: String = this.headers(column)
    val classes: java.util.Map[String, Integer] = enumerateClasses(column)
    insertColumns(column + 1, classes.size - 1)
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      val index: Int = classes.get(obj(column).toString)
      val classCount: Int = classes.size
      for(i <- 0 until classCount) {
        val value = if (i == index) onValue else offValue
        obj(column + i) = value.asInstanceOf[AnyRef]
      }
    }
    for(i <- 0 until classes.size)
      this.headers(column + i) = name + "-" + i
    classes
  }

  /**
   * Use equilateral encoding to encode a column, use zero for the off value and one for the on value.
   * <p/>
   * http://www.heatonresearch.com/wiki/Equilateral
   *
   * @param column The column to encode.
   * @return The column to index mapping (the same result as calling enumerateClasses).
   */
  def encodeEquilateral(column: Int): java.util.Map[String, Integer] = encodeEquilateral(column, 0, 1)

  /**
   * Use equilateral encoding to encode a column, use zero for the off value and one for the on value.
   * <p/>
   * http://www.heatonresearch.com/wiki/Equilateral
   *
   * @param column   The column to use.
   * @param offValue The off value to use.
   * @param onValue  The on value to use.
   * @return The column to index mapping (the same result as calling enumerateClasses).
   */
  def encodeEquilateral(column: Int, offValue: Double, onValue: Double): java.util.Map[String, Integer] = {
    val name: String = this.headers(column)
    val classes: java.util.Map[String, Integer] = enumerateClasses(column)
    val classCount: Int = classes.size
    insertColumns(column + 1, classCount - 1)
    val eq: Equilateral = new Equilateral(classCount, offValue, onValue)
    import scala.collection.JavaConversions._
    for (obj <- this.data) {
      val index: Int = classes.get(obj(column).toString)
      val encoded: Array[Double] = eq.encode(index)
      for(i <- 0 until (classCount -1))
        obj(column + i) = encoded(i).asInstanceOf[AnyRef]
    }
    for(i <- 0 until classes.size) {
      this.headers(column + i) = name + "-" + i
    }
    classes
  }

  /**
   * @return The number of rows.
   */
  def size: Int = data.size

  /**
   * Append new columns to the end of the existing columns.
   *
   * @param count The number of new columns.
   */
  def appendColumns(count: Int) {
    val newHeaders: Array[String] = new Array[String](getHeaderCount + count)
    System.arraycopy(this.headers, 0, newHeaders, 0, getHeaderCount)
    for(i <- 0 until count) {
      newHeaders(i + getHeaderCount) = "new"
    }
    this.headers = newHeaders
    for(rowIndex <- 0 until size) {
      val originalRow: Array[AnyRef] = this.data.get(rowIndex)
      val newRow: Array[AnyRef] = new Array[AnyRef](getHeaderCount)
      System.arraycopy(originalRow, 0, newRow, 0, originalRow.length)
      for(i <- 0 until count) {
        newRow(getHeaderCount - 1 - i) = 0.0.asInstanceOf[AnyRef]
      }
      this.data.remove(rowIndex)
      this.data.add(rowIndex, newRow)
    }
  }

  /**
   * Insert columns at a specific location.
   *
   * @param column      The column to insert BEFORE.
   * @param columnCount The count of columns to insert.
   */
  def insertColumns(column: Int, columnCount: Int) {
    appendColumns(columnCount)
    System.arraycopy(this.headers, column + 1 - columnCount, this.headers, column + 1, getHeaderCount - 1 - column)
    for(i <- 0 until columnCount) {
      this.headers(column + i) = "new"
    }
    import scala.collection.JavaConversions._
    for(obj <- this.data) {
      System.arraycopy(obj, column + 1 - columnCount, obj, column + 1, getHeaderCount - 1 - column)
      for(i <- 0 until columnCount) {
        obj(column + i) = 0.0.asInstanceOf[AnyRef]
      }
    }
  }

  override def equals(other: Any): Boolean = other match {
    case otherSet : DataSet =>
      if (getHeaderCount != otherSet.getHeaderCount) {
        return false
      }
      if (size != otherSet.size) {
        return false
      }
      for(i <- 0 until getHeaderCount) {
        if (!(this.headers(i) == otherSet.getHeaders(i))) {
          return false
        }
      }

      for(i <- 0 until size) {
        val row1: Array[AnyRef] = this.data.get(i)
        val row2: Array[AnyRef] = otherSet.getData.get(i)
        for(j <- 0 until getHeaderCount) {
          if (!(row1(j) == row2(j))) {
            return false
          }
        }
      }
      true
    case _ => false
  }

  /**
   * Extract and label an unsupervised training set.
   *
   * @param labelIndex The column index to use for the label.
   * @return The training set.
   */
  def extractUnsupervisedLabeled(labelIndex: Int): java.util.List[BasicData] = {
    val result: java.util.List[BasicData] = new java.util.ArrayList[BasicData]
    val dimensions: Int = getHeaderCount - 1
    for(rowIndex <- 0 until size) {
      val raw: Array[AnyRef] = this.data.get(rowIndex)
      val row: BasicData = new BasicData(dimensions, 0, raw(labelIndex).toString)
      var colIndex: Int = 0
      for(rawColIndex <- 0 until getHeaderCount) {
        if (rawColIndex != labelIndex) {
          row.input(colIndex) = convertNumeric(raw, rawColIndex)
          colIndex += 1
        }
      }
      result.add(row)
    }
    result
  }

  /**
   * Extract a supervised training set.  This has both input and expected (ideal) output.
   *
   * @param inputBegin The first input column.
   * @param inputCount The number of columns for input.
   * @param idealBegin The first ideal column.
   * @param idealCount The number of columns for ideal.
   * @return The training set.
   */
  def extractSupervised(inputBegin: Int, inputCount: Int, idealBegin: Int, idealCount: Int): java.util.List[BasicData] = {
    val result: java.util.List[BasicData] = new java.util.ArrayList[BasicData]
    for(rowIndex <- 0 until size) {
      val raw: Array[AnyRef] = this.data.get(rowIndex)
      val row: BasicData = new BasicData(inputCount, idealCount)
      for(i <- 0 until inputCount) {
        row.input(i) = convertNumeric(raw, inputBegin + i)
      }
      for(i <- 0 until idealCount) {
        row.ideal(i) = convertNumeric(raw, idealBegin + i)
      }
      result.add(row)
    }
    result
  }

  /**
   * Delete all rows that contain unknown data.  An unknown column has a "?" value.
   */
  def deleteUnknowns() {
    var rowIndex: Int = 0
    while (rowIndex < this.data.size) {
      val row: Array[AnyRef] = data.get(rowIndex)
      var remove: Boolean = false
      class BreakException extends Exception
      try {
        for (aRow <- row) {
          if (aRow.toString == "?") {
            remove = true
            throw new BreakException
          }
        }
      } catch {
        case _ : BreakException =>
      }
      if (remove) {
        data.remove(rowIndex)
      }
      else {
        rowIndex += 1
      }
    }
  }

  /**
   * Delete the specified column.
   *
   * @param col The column to delete.
   */
  def deleteColumn(col: Int) {
    val headers2: Array[String] = new Array[String](headers.length - 1)
    var h2Index: Int = 0
    for(i <- 0 until headers.length) {
      if (i != col) {
        headers2(h2Index) = headers(i)
        h2Index += 1
      }
    }

    this.headers = headers2
    var rowIndex: Int = 0
    import scala.collection.JavaConversions._
    for (row <- this.data) {
      val row2: Array[AnyRef] = new Array[AnyRef](headers.length)
      var r2Index: Int = 0
      for(i <- 0 until headers.length) {
        if (i != col) {
          row2(r2Index) = row(i)
          r2Index += 1
        }

      }
      this.data.set(rowIndex, row2)
      rowIndex += 1
    }
  }

  /**
   * Replace all of the specified values in a column.
   *
   * @param columnIndex The column index.
   * @param searchFor   What to search for.
   * @param replaceWith What to replace with.
   * @param others      What to fill in the others with that do not match.
   */
  def replaceColumn(columnIndex: Int, searchFor: Double, replaceWith: Double, others: Double) {
    import scala.collection.JavaConversions._
    for (row <- this.data) {
      val d: Double = convertNumeric(row, columnIndex)
      if (Math.abs(d - searchFor) < 0.0001) {
        row(columnIndex) = replaceWith.asInstanceOf[AnyRef]
      }
      else {
        row(columnIndex) = others.asInstanceOf[AnyRef]
      }
    }
  }

  /**
   * The data loaded from a CSV, or other source.
   */
  private val data: java.util.List[Array[AnyRef]] = new java.util.ArrayList[Array[AnyRef]]
  /**
   * The column headers.
   */
  private var headers: Array[String] = null
  /**
   * The number formatter to use for this format.
   */
  private val numberFormatter: NumberFormat = NumberFormat.getInstance(Locale.US)
}