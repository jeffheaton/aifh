// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using AIFH_Vol3.Core.General.Data;
using CsvHelper;

namespace AIFH_Vol3.Core.Normalize
{
    /// <summary>
    ///     Holds a data set.  This is usually loaded from a CSV.  It can also be generated.
    /// </summary>
    public class DataSet
    {
        /// <summary>
        ///     The data loaded from a CSV, or other source.
        /// </summary>
        private readonly IList<object[]> _data = new List<object[]>();

        /// <summary>
        ///     The column headers.
        /// </summary>
        private string[] _headers;

        /// <summary>
        ///     Create a blank data set.
        /// </summary>
        /// <param name="theHeaders">The column headers.</param>
        public DataSet(string[] theHeaders)
        {
            _headers = theHeaders;
        }

        /// <summary>
        ///     The number of columns (or headers).
        /// </summary>
        public int HeaderCount
        {
            get { return _headers.Length; }
        }

        /// <summary>
        ///     The column headers.
        /// </summary>
        public string[] Headers
        {
            get { return _headers; }
        }

        /// <summary>
        ///     The row data.
        /// </summary>
        public IList<object[]> Data
        {
            get { return _data; }
        }

        /**
         * @return The number of rows.
         */

        public int Count
        {
            get { return _data.Count; }
        }

        /// <summary>
        ///     Convert a column to numeric.  Save the new Double object in place of the string.
        /// </summary>
        /// <param name="obj">The column array.</param>
        /// <param name="column">The column to change.</param>
        /// <returns>The numeric value.</returns>
        private static double ConvertNumeric(object[] obj, int column)
        {
            double x;
            if (obj[column] is double)
            {
                x = (double) obj[column];
            }
            else
            {
                try
                {
                    x = double.Parse(obj[column].ToString(), CultureInfo.InvariantCulture);
                    obj[column] = x;
                }
                catch (FormatException e)
                {
                    throw new AIFHError("Bad number", e);
                }
            }

            return x;
        }

        /// <summary>
        ///     Load a CSV file from a file.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <returns>The data set read.</returns>
        public static DataSet Load(string filename)
        {
            using (var fileReader = File.OpenText(filename))
            {
                return Load(fileReader);
            }
        }

        /// <summary>
        ///     Load a CSV from an input stream.
        /// </summary>
        /// <param name="stream">The input stream.</param>
        /// <returns>The loaded file.</returns>
        public static DataSet Load(StreamReader stream)
        {
            DataSet result = null;
            using (var csvReader = new CsvReader(stream))
            {
                var fieldCount = 0;

                while (csvReader.Read())
                {
                    // if we just read the first row, then we need to read
                    // the headers, they were already grabbed.
                    if (result == null)
                    {
                        fieldCount = csvReader.FieldHeaders.Count();
                        var headers = new string[fieldCount];

                        for (var i = 0; i < fieldCount; i++)
                        {
                            headers[i] = csvReader.FieldHeaders[i];
                        }

                        result = new DataSet(headers);
                    }

                    // process each line
                    var obj = new object[fieldCount];
                    for (var i = 0; i < fieldCount; i++)
                    {
                        obj[i] = csvReader.GetField<string>(i);
                    }
                    result.Add(obj);
                }
            }

            return result;
        }

        /// <summary>
        ///     Save the specified data set to a CSV file.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <param name="ds">The data set to save.</param>
        public static void Save(string filename, DataSet ds)
        {
            using (var textWriter = File.CreateText(filename))
            {
                Save(textWriter, ds);
            }
        }

        /// <summary>
        ///     Save the specified data to an output stream.
        /// </summary>
        /// <param name="textWriter">The output stream.</param>
        /// <param name="ds">The data set.</param>
        public static void Save(TextWriter textWriter, DataSet ds)
        {
            using (var writer = new CsvWriter(textWriter))
            {
                // write the headers
                foreach (var header in ds.Headers)
                {
                    writer.WriteField(header);
                }
                writer.NextRecord();

                // write the data
                foreach (var item in ds.Data)
                {
                    for (var i = 0; i < ds.HeaderCount; i++)
                    {
                        writer.WriteField(item[i].ToString());
                    }
                    writer.NextRecord();
                }
            }
        }

        /// <summary>
        ///     Add a row.
        /// </summary>
        /// <param name="row">The row to add.</param>
        public void Add(object[] row)
        {
            _data.Add(row);
        }

        /// <summary>
        ///     Get the maximum numeric value for a column.
        /// </summary>
        /// <param name="column">The column.</param>
        /// <returns>The max numeric value.</returns>
        public double GetMax(int column)
        {
            return _data.Select(obj => ConvertNumeric(obj, column)).Concat(new[] {double.NegativeInfinity}).Max();
        }

        /// <summary>
        ///     Get the minimum numeric value for a column.
        /// </summary>
        /// <param name="column">The column.</param>
        /// <returns>The min numeric value.</returns>
        public double GetMin(int column)
        {
            return _data.Select(obj => ConvertNumeric(obj, column)).Concat(new[] {double.PositiveInfinity}).Min();
        }

        /// <summary>
        ///     Normalize a column using range normalization.
        ///     http://www.heatonresearch.com/wiki/Range_Normalization
        /// </summary>
        /// <param name="column">The column to normalize.</param>
        /// <param name="dataLow">The low value for the actual data.</param>
        /// <param name="dataHigh">The high value for the actual data.</param>
        /// <param name="normalizedLow">The desired low normalized value.</param>
        /// <param name="normalizedHigh">The desired high normalized value.</param>
        public void NormalizeRange(int column, double dataLow, double dataHigh, double normalizedLow,
            double normalizedHigh)
        {
            foreach (var obj in _data)
            {
                var x = ConvertNumeric(obj, column);

                obj[column] = (x - dataLow)
                              /(dataHigh - dataLow)
                              *(normalizedHigh - normalizedLow) + normalizedLow;
            }
        }

        /// <summary>
        ///     Normalize a column using range normalization.  Automatically determine the actual data high and low.
        ///     http://www.heatonresearch.com/wiki/Range_Normalization
        /// </summary>
        /// <param name="column">The column to normalize.</param>
        /// <param name="normalizedLow">The desired low normalized value.</param>
        /// <param name="normalizedHigh">The desired high normalized value.</param>
        public void NormalizeRange(int column, double normalizedLow, double normalizedHigh)
        {
            var dataLow = GetMin(column);
            var dataHigh = GetMax(column);
            NormalizeRange(column, dataLow, dataHigh, normalizedLow, normalizedHigh);
        }

        /// <summary>
        ///     De-Normalize a column using range normalization.
        ///     http://www.heatonresearch.com/wiki/Range_Normalization
        /// </summary>
        /// <param name="column">The column to normalize.</param>
        /// <param name="dataLow">The low value for the actual data.</param>
        /// <param name="dataHigh">The high value for the actual data.</param>
        /// <param name="normalizedLow">The desired low normalized value.</param>
        /// <param name="normalizedHigh">The desired high normalized value.</param>
        public void DeNormalizeRange(int column, double dataLow, double dataHigh, double normalizedLow,
            double normalizedHigh)
        {
            foreach (var obj in _data)
            {
                var x = ConvertNumeric(obj, column);

                obj[column] = ((dataLow - dataHigh)*x - normalizedHigh
                               *dataLow + dataHigh*normalizedLow)
                              /(normalizedLow - normalizedHigh);
            }
        }

        /// <summary>
        ///     Normalize a column using reciprocal normalization.
        ///     http://www.heatonresearch.com/wiki/Reciprocal_Normalization
        /// </summary>
        /// <param name="column">The column to encode.</param>
        public void NormalizeReciprocal(int column)
        {
            foreach (var obj in _data)
            {
                var x = ConvertNumeric(obj, column);
                obj[column] = 1/x;
            }
        }

        /// <summary>
        ///     De-Normalize a column using reciprocal normalization.
        ///     Note: normalization and de-normalization are the same mathematical operation.
        ///     http://www.heatonresearch.com/wiki/Reciprocal_Normalization
        /// </summary>
        /// <param name="column">The column to encode.</param>
        public void DeNormalizeReciprocal(int column)
        {
            NormalizeReciprocal(column);
        }

        /// <summary>
        ///     Enumerate classes (factors) into a numbered set.
        /// </summary>
        /// <param name="column">The column to enumerate.</param>
        /// <returns>The numbered set.</returns>
        public IDictionary<string, int> EnumerateClasses(int column)
        {
            // determine classes
            var classes = new HashSet<string>();
            foreach (var obj in _data)
            {
                classes.Add(obj[column].ToString());
            }
            // assign numeric values to each class
            IDictionary<string, int> result = new Dictionary<string, int>();
            var index = 0;
            foreach (var className in classes)
            {
                result[className] = index++;
            }

            return result;
        }

        /// <summary>
        ///     Encode (enumerate) a column with simple numeric index encoding.
        /// </summary>
        /// <param name="column">The column to encode.</param>
        /// <returns>The mapping from column names to indexes.</returns>
        public IDictionary<string, int> EncodeNumeric(int column)
        {
            var classes = EnumerateClasses(column);

            foreach (var obj in _data)
            {
                var index = classes[obj[column].ToString()];
                obj[column] = index;
            }

            return classes;
        }

        /// <summary>
        ///     Encode a column using "one of n" encoding.  Use 0 for the off value, and 1 for on.
        ///     http://www.heatonresearch.com/wiki/One_of_n
        /// </summary>
        /// <param name="column">The column to use.</param>
        /// <returns>The column to index mapping (the same result as calling enumerateClasses).</returns>
        public IDictionary<string, int> EncodeOneOfN(int column)
        {
            return EncodeOneOfN(column, 0, 1);
        }

        /// <summary>
        ///     Encode a column using "one of n" encoding.
        ///     http://www.heatonresearch.com/wiki/One_of_n
        /// </summary>
        /// <param name="column">The column to use.</param>
        /// <param name="offValue">The off value to use.</param>
        /// <param name="onValue">The on value to use.</param>
        /// <returns>The column to index mapping (the same result as calling enumerateClasses).</returns>
        public IDictionary<string, int> EncodeOneOfN(int column, double offValue, double onValue)
        {
            // remember the column name
            var name = _headers[column];

            // make space for it
            var classes = EnumerateClasses(column);
            InsertColumns(column + 1, classes.Count - 1);

            // perform the 1 of n encode
            foreach (var obj in _data)
            {
                var index = classes[obj[column].ToString()];
                var classCount = classes.Count;

                for (var i = 0; i < classCount; i++)
                {
                    obj[column + i] = i == index ? onValue : offValue;
                }
            }

            // name the new columns
            for (var i = 0; i < classes.Count; i++)
            {
                _headers[column + i] = name + "-" + i;
            }

            return classes;
        }

        /**
 * 
 *
 * @param column 
 * @return 
 */

        /// <summary>
        ///     Use equilateral encoding to encode a column, use zero for the off value and one for the on value.
        ///     http://www.heatonresearch.com/wiki/Equilateral
        /// </summary>
        /// <param name="column">The column to encode.</param>
        /// <returns>The column to index mapping (the same result as calling enumerateClasses).</returns>
        public IDictionary<string, int> EncodeEquilateral(int column)
        {
            return EncodeEquilateral(column, 0, 1);
        }

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

        public IDictionary<string, int> EncodeEquilateral(int column, double offValue, double onValue)
        {
            // remember the column name
            var name = _headers[column];

            // make space for it
            var classes = EnumerateClasses(column);
            var classCount = classes.Count;
            InsertColumns(column + 1, classCount - 1);

            // perform the equilateral
            var eq = new Equilateral(classCount, offValue, onValue);

            foreach (var obj in _data)
            {
                var index = classes[obj[column].ToString()];

                var encoded = eq.Encode(index);

                for (var i = 0; i < classCount - 1; i++)
                {
                    obj[column + i] = encoded[i];
                }
            }

            // name the new columns
            for (var i = 0; i < classes.Count; i++)
            {
                _headers[column + i] = name + "-" + i;
            }

            return classes;
        }

        /**
         * Append new columns to the end of the existing columns.
         *
         * @param count The number of new columns.
         */

        public void AppendColumns(int count)
        {
            // add the headers
            var newHeaders = new string[HeaderCount + count];
            Array.Copy(_headers, 0, newHeaders, 0, HeaderCount);

            for (var i = 0; i < count; i++)
            {
                newHeaders[i + HeaderCount] = "new";
            }

            _headers = newHeaders;

            // add the data
            for (var rowIndex = 0; rowIndex < Count; rowIndex++)
            {
                var originalRow = _data[rowIndex];
                var newRow = new object[HeaderCount];
                Array.Copy(originalRow, 0, newRow, 0, originalRow.Length);
                for (var i = 0; i < count; i++)
                {
                    newRow[HeaderCount - 1 - i] = (double) 0;
                }
                _data.RemoveAt(rowIndex);
                _data.Insert(rowIndex, newRow);
            }
        }

        /// <inheritdoc />
        public void InsertColumns(int column, int columnCount)
        {
            // create space for new columns
            AppendColumns(columnCount);

            // insert headers
            Array.Copy(_headers, column + 1 - columnCount, _headers, column + 1, HeaderCount - 1 - column);

            // mark new columns headers
            for (var i = 0; i < columnCount; i++)
            {
                _headers[column + i] = "new";
            }

            foreach (var obj in _data)
            {
                // insert columns
                Array.Copy(obj, column + 1 - columnCount, obj, column + 1, HeaderCount - 1 - column);

                // mark new columns
                for (var i = 0; i < columnCount; i++)
                {
                    obj[column + i] = (double) 0;
                }
            }
        }

        /// <inheritdoc />
        public override bool Equals(object other)
        {
            if (!(other is DataSet))
            {
                return false;
            }

            var otherSet = (DataSet) other;

            // do the basic sizes match

            if (HeaderCount != otherSet.HeaderCount)
            {
                return false;
            }

            if (Count != otherSet.Count)
            {
                return false;
            }

            // do the headers match?
            for (var i = 0; i < HeaderCount; i++)
            {
                if (!_headers[i].Equals(otherSet.Headers[i]))
                {
                    return false;
                }
            }

            // does the data match?
            for (var i = 0; i < Count; i++)
            {
                var row1 = _data[i];
                var row2 = ((DataSet) other).Data[i];

                for (var j = 0; j < HeaderCount; j++)
                {
                    if (!row1[j].Equals(row2[j]))
                    {
                        return false;
                    }
                }
            }


            return true;
        }

        /// <summary>
        ///     Extract and label an unsupervised training set.
        /// </summary>
        /// <param name="labelIndex">The column index to use for the label.</param>
        /// <returns>The training set.</returns>
        public IList<BasicData> ExtractUnsupervisedLabeled(int labelIndex)
        {
            IList<BasicData> result = new List<BasicData>();

            var dimensions = HeaderCount - 1;

            for (var rowIndex = 0; rowIndex < Count; rowIndex++)
            {
                var raw = _data[rowIndex];
                var row = new BasicData(dimensions, 0, raw[labelIndex].ToString());

                var colIndex = 0;
                for (var rawColIndex = 0; rawColIndex < HeaderCount; rawColIndex++)
                {
                    if (rawColIndex != labelIndex)
                    {
                        row.Input[colIndex++] = ConvertNumeric(raw, rawColIndex);
                    }
                }

                result.Add(row);
            }

            return result;
        }

        /// <summary>
        ///     Extract a supervised training set.  This has both input and expected (ideal) output.
        /// </summary>
        /// <param name="inputBegin">The first input column.</param>
        /// <param name="inputCount">The number of columns for input.</param>
        /// <param name="idealBegin">The first ideal column.</param>
        /// <param name="idealCount">The number of columns for ideal.</param>
        /// <returns>The training set.</returns>
        public IList<BasicData> ExtractSupervised(int inputBegin, int inputCount, int idealBegin, int idealCount)
        {
            IList<BasicData> result = new List<BasicData>();

            for (var rowIndex = 0; rowIndex < Count; rowIndex++)
            {
                var raw = _data[rowIndex];
                var row = new BasicData(inputCount, idealCount);

                for (var i = 0; i < inputCount; i++)
                {
                    row.Input[i] = ConvertNumeric(raw, inputBegin + i);
                }

                for (var i = 0; i < idealCount; i++)
                {
                    row.Ideal[i] = ConvertNumeric(raw, idealBegin + i);
                }

                result.Add(row);
            }

            return result;
        }

        /// <summary>
        ///     Delete all rows that contain unknown data.  An unknown column has a "?" value.
        /// </summary>
        public void DeleteUnknowns()
        {
            var rowIndex = 0;
            while (rowIndex < _data.Count)
            {
                var row = _data[rowIndex];
                var remove = row.Any(aRow => aRow.ToString().Equals("?"));

                if (remove)
                {
                    _data.RemoveAt(rowIndex);
                }
                else
                {
                    rowIndex++;
                }
            }
        }

        /// <summary>
        ///     Delete the specified column.
        /// </summary>
        /// <param name="col">The column to delete.</param>
        public void DeleteColumn(int col)
        {
            var headers2 = new string[_headers.Length - 1];

            // first, remove the header
            var h2Index = 0;
            for (var i = 0; i < _headers.Length; i++)
            {
                if (i != col)
                {
                    headers2[h2Index++] = _headers[i];
                }
            }
            _headers = headers2;

            // now process the data
            for (var rowIndex = 0; rowIndex < _data.Count; rowIndex++)
            {
                var row = _data[rowIndex];
                var row2 = new object[_headers.Length];
                var r2Index = 0;
                for (var i = 0; i <= _headers.Length; i++)
                {
                    if (i != col)
                    {
                        row2[r2Index++] = row[i];
                    }
                }
                _data[rowIndex] = row2;
            }
        }

        /// <summary>
        ///     Replace all of the specified values in a column.
        /// </summary>
        /// <param name="columnIndex">The column index.</param>
        /// <param name="searchFor">What to search for.</param>
        /// <param name="replaceWith">What to replace with.</param>
        /// <param name="others">What to fill in the others with that do not match.</param>
        public void ReplaceColumn(int columnIndex, double searchFor, double replaceWith, double others)
        {
            foreach (var row in _data)
            {
                var d = ConvertNumeric(row, columnIndex);
                if (Math.Abs(d - searchFor) < 0.0001)
                {
                    row[columnIndex] = replaceWith;
                }
                else
                {
                    row[columnIndex] = others;
                }
            }
        }

        public void NormalizeZScore(int column)
        {
            var standardDeviation = GetStandardDeviation(column);
            var mean = GetMean(column);

            foreach (var obj in _data)
            {
                if (IsMissing(obj[column].ToString()))
                {
                    obj[column] = 0; // Place at mean
                }
                else
                {
                    var x = ConvertNumeric(obj, column);
                    obj[column] = (x - mean)/standardDeviation;
                }
            }
        }

        /// <summary>
        ///     Determine if the specified value is missing (empty string, NULL, NA, or ?).
        /// </summary>
        /// <param name="str">The value to check.</param>
        /// <returns>True if missing.</returns>
        public static bool IsMissing(string str)
        {
            return str.Equals("?") || str.Trim().Equals("") || str.Trim().ToUpper().Equals("NA")
                   || str.Trim().ToUpper().Equals("NULL");
        }

        /// <summary>
        ///     Get the mean numeric value for a column.
        /// </summary>
        /// <param name="column">The column.</param>
        /// <returns>The mean numeric value.</returns>
        public double GetMean(int column)
        {
            double sum = 0;
            var count = 0;

            foreach (var obj in _data)
            {
                if (!IsMissing(obj[column].ToString()))
                {
                    sum += ConvertNumeric(obj, column);
                    count++;
                }
            }

            return sum/count;
        }

        /// <summary>
        ///     Get the standard deviation value for a column.
        /// </summary>
        /// <param name="column">The column.</param>
        /// <returns>The standard deviation numeric value.</returns>
        public double GetStandardDeviation(int column)
        {
            var mean = GetMean(column);
            double sum = 0;
            var count = 0;

            foreach (var obj in _data)
            {
                if (!IsMissing(obj[column].ToString()))
                {
                    var delta = mean - ConvertNumeric(obj, column);
                    sum += delta*delta;
                    count++;
                }
            }

            return Math.Sqrt(sum/count);
        }
    }
}