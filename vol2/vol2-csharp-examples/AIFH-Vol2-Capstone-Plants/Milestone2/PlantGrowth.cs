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
using AIFH_Vol2.Core;
using AIFH_Vol2.Core.Distance;
using AIFH_Vol2_Capstone_Plants.Milestone1;

namespace AIFH_Vol2_Capstone_Plants.Milestone2
{
    /// <summary>
    ///     This class provides a growth cycle for the plant genome.  This class runs the "program" embedded in the plant's
    ///     genome.  A plant genome is an array of four vectors.  Each vector is of length 4, so the entire genome is
    ///     4*4 = 16 double values.
    ///     Each of the four vectors corresponds to a cell's info vector.  A cell info vector provides information about the
    ///     state of a cell.  By cell, I mean a grid cell.  The grid cell can be either living (filled) or dead (empty).
    ///     The info vector for a cell is calculated as follows.
    ///     Element 0: The height of the cell. 1.0 for the last row and 0.0 for the first row.
    ///     Element 1: The amount of sunlight (for surface cells) or water (for underground cells) exposure for this cell.
    ///     Element 2: Crowding by neighbors.
    ///     Element 3: Nourishment for this cell.
    ///     The genome's vectors are as follows:
    ///     Vector 0: Stem desired
    ///     Vector 1: Leaf desired
    ///     Vector 2: Growth Option #1
    ///     Vector 3: Growth Option #2
    ///     Vectors 0 and 1 go together.  For each living cell we see if its info vector is closer to vector 0 or vector 1.
    ///     If it is closer to stem (0), then the leafyness attribute of the cell is decreased.  Leaf's can only move towards
    ///     stem.  A stem cannot change back into a leaf.
    ///     Vectors 2 and 3 also go together. When a plant cell is eligible for growth, it evaluates all neighbor cells to
    ///     see which it wants to grow into.  What ever neighbor cell is closest to ether vector 2 or 3 is chosen.  If
    ///     the candidate cell is not lower than a specific threshold to either, then no growth occurs.
    ///     A ratio between how leafy the surface is, and roots must also be maintained for growth to occur.
    /// </summary>
    public class PlantGrowth
    {
        /// <summary>
        ///     Transformations to move from a cell to the 9 neighboring cells.
        ///     These are the column values.
        /// </summary>
        private readonly int[] _colTransform = {0, 0, -1, 1, -1, 1, 1, -1};

        /// <summary>
        ///     Euclidean distance is used to calculate distances to the genome vectors.
        /// </summary>
        private readonly EuclideanDistance _dist = new EuclideanDistance();

        /// <summary>
        ///     Used to hold the new cells that have grown.
        /// </summary>
        private readonly bool[][] _newComposition;

        /// <summary>
        ///     Transformations to move from a cell to the 9 neighboring cells.
        ///     These are the row values.
        /// </summary>
        private readonly int[] _rowTransform = {-1, 1, 0, 0, -1, 1, -1, 1};

        /// <summary>
        ///     Constructor.
        /// </summary>
        public PlantGrowth()
        {
            _newComposition = new bool[PlantUniverse.UniverseHeight][];
            for (int i = 0; i < _newComposition.Length; i++)
            {
                _newComposition[i] = new bool[PlantUniverse.UniverseWidth];
            }
        }

        /// <summary>
        ///     Calculate the growth potential for a candidate cell. Evaluates the distance between the candidate cell's info
        ///     vector and the two growth vectors in the genome.  The minimum of these two vectors will be returned if
        ///     it is below a specified minimum threshold.
        /// </summary>
        /// <param name="universe">The universe to evaluate.</param>
        /// <param name="row">The row to evaluate.</param>
        /// <param name="col">The column to evaluate.</param>
        /// <param name="genome">The genome.</param>
        /// <returns>The minimum distance.</returns>
        private double GetGrowthPotential(PlantUniverse universe, int row, int col, double[] genome)
        {
            double[] cellVec = universe.GetCellInfoVector(row, col);
            double d1 = _dist.Calculate(cellVec, 0, genome, PlantUniverse.CellVectorLength*2,
                PlantUniverse.CellVectorLength);
            double d2 = _dist.Calculate(cellVec, 0, genome, PlantUniverse.CellVectorLength*3,
                PlantUniverse.CellVectorLength);

            double result = Math.Min(d1, d2);
            if (result > PlantUniverse.MinGrowthDist)
            {
                result = -1;
            }

            return result;
        }

        /// <summary>
        ///     Evaluate neighbors to see where to grow into.
        /// </summary>
        /// <param name="universe">The universe.</param>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <param name="genome">The genome.</param>
        /// <param name="allowRoot">Are roots allowed?</param>
        /// <param name="allowSurface">Is surface growth allowed.</param>
        private void EvaluateNeighbors(PlantUniverse universe, int row, int col, double[] genome, bool allowRoot,
            bool allowSurface)
        {
            int growthTargetRow = row;
            int growthTargetCol = col;
            double growthTargetScore = double.PositiveInfinity;

            for (int i = 0; i < _colTransform.Length; i++)
            {
                int evalCol = col + _colTransform[i];
                int evalRow = row + _rowTransform[i];

                if (!allowRoot && evalRow >= PlantUniverse.GroundLine)
                {
                    continue;
                }

                if (!allowSurface && evalRow < PlantUniverse.GroundLine)
                {
                    continue;
                }

                if (universe.IsValid(evalRow, evalCol))
                {
                    double p = GetGrowthPotential(universe, evalRow, evalCol, genome);
                    if (p > 0)
                    {
                        if (p < growthTargetScore)
                        {
                            growthTargetScore = p;
                            growthTargetRow = evalRow;
                            growthTargetCol = evalCol;
                        }
                    }
                }
            }

            // Grow new cell, if requested, did we ever set target row & col to anything?
            if (growthTargetRow != row || growthTargetCol != col)
            {
                _newComposition[growthTargetRow][growthTargetCol] = true;
            }
        }

        /// <summary>
        ///     Run a growth cycle for the universe.
        /// </summary>
        /// <param name="universe">The universe.</param>
        /// <param name="genome">The genome.</param>
        public void RunGrowth(PlantUniverse universe, double[] genome)
        {
            // Does this plant have enough roots to grow?
            if (universe.SurfaceCount < AIFH.DefaultPrecision)
            {
                return;
            }

            // The amount of leafy material per root nourishment.  A higher number indicates
            // more root nourishment than leafs.
            double rootRatio = universe.RootCount/universe.SurfaceCount;

            bool allowRoot = rootRatio < 0.5; //rootRatio < 0.1;
            bool allowSurface = rootRatio > 0.5;

            // Reset the new composition to be the composition of the current universe
            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    _newComposition[row][col] = false;
                }
            }

            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    PlantUniverseCell cell = universe.GetCell(row, col);

                    // see if we want to change the composition
                    if (row < PlantUniverse.GroundLine)
                    {
                        double[] cellVec = universe.GetCellInfoVector(row, col);
                        double d1 = _dist.Calculate(cellVec, 0, genome, 0, PlantUniverse.CellVectorLength);
                        double d2 = _dist.Calculate(cellVec, 0, genome, PlantUniverse.CellVectorLength,
                            PlantUniverse.CellVectorLength);

                        if (d1 < d2)
                        {
                            cell.Leafyness = (cell.Leafyness*PlantUniverse.StemTransition);
                        }
                    }

                    // Evaluate growth into each neighbor cell
                    if (universe.CanGrow(row, col))
                    {
                        EvaluateNeighbors(universe, row, col, genome, allowRoot, allowSurface);
                    }
                }
            }

            // Copy the new composition back to the universe
            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    PlantUniverseCell cell = universe.GetCell(row, col);

                    if (_newComposition[row][col])
                    {
                        cell.Leafyness = row >= PlantUniverse.GroundLine ? 0 : 1.0;
                        cell.Energy = 1.0;
                        cell.Nourishment = 1.0;
                    }
                }
            }
        }
    }
}
