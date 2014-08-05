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
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2_MergePhysics.Universe
{
    public class UniverseHolder : ICloneable
    {
        /// <summary>
        ///     The cell size.
        /// </summary>
        private readonly int _cellSize;

        /// <summary>
        ///     The universe.
        /// </summary>
        private readonly UniverseCell[][] _data;


        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="height">The universe height.</param>
        /// <param name="width">The universe width.</param>
        /// <param name="theSize">The number of dimensions in a universe cell.</param>
        public UniverseHolder(int height, int width, int theSize)
        {
            _cellSize = theSize;
            _data = new UniverseCell[height][];
            for (int row = 0; row < height; row++)
            {
                _data[row] = new UniverseCell[width];
                for (int col = 0; col < width; col++)
                {
                    _data[row][col] = new UniverseCell(theSize);
                }
            }
        }

        public int CellSize
        {
            get { return _cellSize; }
        }

        /**
         * @return The universe grid.
         */

        public UniverseCell[][] Data
        {
            get { return _data; }
        }

        /**
         * @return The height of the universe.
         */

        public int Height
        {
            get { return _data.Length; }
        }

        /// <summary>
        ///     The width of the universe.
        /// </summary>
        public int Width
        {
            get { return _data[0].Length; }
        }

        /**
         * {@inheritDoc}
         */

        public Object Clone()
        {
            var result = new UniverseHolder(Height, Width,
                _cellSize);
            result.Copy(this);
            return result;
        }

        /// <summary>
        ///     Add the specified value to the specified dimension of the specified cell.
        /// </summary>
        /// <param name="row">The cell's row.</param>
        /// <param name="col">The cell's column.</param>
        /// <param name="i">The index of the dimension to add to.</param>
        /// <param name="d">The value to add to the cell.</param>
        public void Add(int row, int col, int i, double d)
        {
            _data[row][col].Add(i, d);
        }

        /// <summary>
        /// Compare this universe to another and return the difference.  A value of zero indicates an identical universe.
        /// The lower the value, the more similar.
        /// </summary>
        /// <param name="otherUniverse">The other universe.</param>
        /// <returns>The difference between the universes.</returns>
        public double Compare(UniverseHolder otherUniverse)
        {
            int result = 0;
            int total = 0;
            for (int row = 0; row < otherUniverse.Height; row++)
            {
                for (int col = 0; col < otherUniverse.Width; col++)
                {
                    int d1 = Math.Abs((int) (255*Get(row, col).Avg));
                    int d2 = Math.Abs((int) (255*otherUniverse.Get(row, col).Avg));
                    if (Math.Abs(d1 - d2) > 10)
                    {
                        result++;
                    }
                    total++;
                }
            }

            return result/(double) total;
        }

        /// <summary>
        /// Copy another universe into this one.
        /// </summary>
        /// <param name="source">The source universe.</param>
        public void Copy(UniverseHolder source)
        {
            for (int row = 0; row < Height; row++)
            {
                for (int col = 0; col < Width; col++)
                {
                    for (int i = 0; i < _cellSize; i++)
                    {
                        _data[row][col].Data[i] = source.Get(row, col).Data[i];
                    }
                }
            }
        }

        /// <summary>
        /// Get the universe cell for the specified row and column.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The universe cell.</returns>
        public UniverseCell Get(int row, int col)
        {
            return _data[row][col];
        }

        /// <summary>
        /// Get the universe cell for the specified row, column and index.
        /// </summary>
        /// <param name="row">The row of the cell.</param>
        /// <param name="col">The column of the cell.</param>
        /// <param name="i">The index (dimension) inside the cell.</param>
        /// <returns>The value.</returns>
        public double Set(int row, int col, int i)
        {
            return _data[row][col].Data[i];
        }

        /**
         * The number of dimensions inside a cell.
         *
         * @return The size of a cell.
         */

        /// <summary>
        ///     Determine if row and col are valid.  Both must be above zero and within the height and width.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>True, if valid.</returns>
        public bool IsValid(int row, int col)
        {
            return !(row < 0 || col < 0 || row >= Height || col >= Width);
        }

        /// <summary>
        ///     Randomize the universe.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        public void Randomize(IGenerateRandom rnd)
        {
            for (int row = 0; row < Height; row++)
            {
                for (int col = 0; col < Width; col++)
                {
                    for (int i = 0; i < 3; i++)
                    {
                        _data[row][col].Randomize(rnd);
                    }
                }
            }
        }
    }
}
