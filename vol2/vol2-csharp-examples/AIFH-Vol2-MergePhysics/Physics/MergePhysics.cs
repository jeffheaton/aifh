// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
using System.Globalization;
using System.IO;
using System.Text;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2_MergePhysics.Universe;

namespace AIFH_Vol2_MergePhysics.Physics
{
    /// <summary>
    ///     Merge physics determines how cells are to be populated.
    ///     For more information on how the physics works see:
    ///     http://www.codeproject.com/Articles/730362/Using-an-evolutionary-algorithm-to-create-a-cellul
    /// </summary>
    public class MergePhysics : IPhysics
    {
        /// <summary>
        ///     The color table.
        /// </summary>
        private readonly double[][] _colorTable =
        {
            new double[] {-1, -1, -1}, new double[] {1, -1, -1},
            new double[] {-1, 1, -1}, new double[] {1, 1, -1}, new double[] {-1, -1, 1}, new double[] {1, -1, 1},
            new double[] {-1, 1, 1}, new double[] {1, 1, 1}
        };


        /// <summary>
        ///     Transformations to move from a cell to the 9 neighboring cells.
        ///     These are the column values.
        /// </summary>
        private readonly int[] _colTransform = {0, 0, -1, 1, -1, 1, 1, -1};

        /// <summary>
        ///     The data that defines the physical constants.
        /// </summary>
        private readonly double[] _data;

        /// <summary>
        ///     The data sorted.
        /// </summary>
        private readonly int[] _dataOrder;

        /// <summary>
        ///     Transformations to move from a cell to the 9 neighboring cells.
        ///     These are the row values.
        /// </summary>
        private readonly int[] _rowTransform = {-1, 1, 0, 0, -1, 1, -1, 1};

        /// <summary>
        ///     The universe.
        /// </summary>
        private readonly UniverseHolder _universe;

        /// <summary>
        ///     Construct the merge physics.
        /// </summary>
        /// <param name="theUniverse">The universe.</param>
        public MergePhysics(UniverseHolder theUniverse)
        {
            _data = new double[2*_colorTable.Length];
            _dataOrder = new int[_colorTable.Length];
            _universe = theUniverse;
        }

        /// <inheritdoc />
        public void CopyData(double[] sourceData)
        {
            Array.Copy(sourceData, 0, _data, 0, _data.Length);
            SortData();
        }

        /// <inheritdoc />
        public double[] Data
        {
            get { return _data; }
        }

        /// <inheritdoc />
        public void Load(String filename)
        {
            using (FileStream fileStream = File.OpenRead(filename))
            using (var streamReader = new StreamReader(fileStream, Encoding.UTF8, true, 128))
            {
                String line;
                while ((line = streamReader.ReadLine()) != null)
                {
                    line = line.Substring(1);
                    line = line.Substring(0, line.Length - 1);
                    string[] tok = line.Split(',');
                    int idx = 0;
                    foreach (string str in tok)
                    {
                        _data[idx++] = double.Parse(str, CultureInfo.InvariantCulture);
                        SortData();
                    }
                }
            }
        }

        /// <inheritdoc />
        public void ProcessPixel(UniverseHolder outputUniverse, int row,
            int col)
        {
            double total = 0;
            int cnt = 0;

            for (int dir = 0; dir < _rowTransform.Length; dir++)
            {
                int otherRow = row + _rowTransform[dir];
                int otherCol = col + _colTransform[dir];
                if (_universe.IsValid(otherRow, otherCol))
                {
                    UniverseCell otherCell = _universe.Get(otherRow,
                        otherCol);
                    total += otherCell.Avg;
                    cnt++;
                }
            }

            total /= cnt;
            for (int i = 0; i < _colorTable.Length; i++)
            {
                int idx = _dataOrder[i];
                if (total < _data[idx*2])
                {
                    for (int j = 0; j < outputUniverse.CellSize; j++)
                    {
                        double d = _colorTable[idx][j]
                                   - _universe.Get(row, col).Data[j];
                        double pct = _data[1 + idx*2];
                        pct = (pct + 1.0)/2.0;
                        d *= pct;
                        outputUniverse.Add(row, col, j, d);
                    }
                    break;
                }
            }
        }

        /// <inheritdoc />
        public void Randomize()
        {
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();
            for (int i = 0; i < _data.Length; i++)
            {
                _data[i] = rnd.NextDouble()*2.0 - 1.0;
            }
            SortData();
        }

        /// <inheritdoc />
        public void Save(string filename)
        {
            using (StreamWriter dest = File.CreateText(filename))
            {
                bool first = true;
                dest.Write("[");

                foreach (double d in _data)
                {
                    if (!first)
                    {
                        dest.Write(",");
                    }
                    dest.Write(d.ToString(CultureInfo.InvariantCulture));
                    first = false;
                }
                dest.WriteLine("]");
            }
        }

        /// <summary>
        ///     Determine if "value" is in the array.
        /// </summary>
        /// <param name="list">The list.</param>
        /// <param name="len">The length.</param>
        /// <param name="value">The value.</param>
        /// <returns>True, if value is in the list.</returns>
        private bool ListContains(int[] list, int len,
            int value)
        {
            for (int i = 0; i < len; i++)
            {
                if (list[i] == value)
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        ///     Sort the physical data for faster lookup.
        /// </summary>
        private void SortData()
        {
            // create the order table
            for (int x = 0; x < _colorTable.Length; x++)
            {
                int lowestIndex = -1;
                for (int y = 0; y < _colorTable.Length; y++)
                {
                    double value = _data[y*2];
                    // Only consider positions that are not already in the list.
                    if (!ListContains(_dataOrder, x, y))
                    {
                        // See if this is the lowest index found for this pass.
                        if (lowestIndex == -1)
                        {
                            lowestIndex = y;
                        }
                        else
                        {
                            if (value < _data[lowestIndex*2])
                            {
                                lowestIndex = y;
                            }
                        }
                    }
                }
                _dataOrder[x] = lowestIndex;
            }
        }
    }
}
