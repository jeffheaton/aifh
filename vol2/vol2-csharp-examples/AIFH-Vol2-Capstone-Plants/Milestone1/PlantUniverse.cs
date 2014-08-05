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

namespace AIFH_Vol2_Capstone_Plants.Milestone1
{
    /// <summary>
    ///     This class holds the grid that is the universe that a single plant grows in.  Each plant has its
    ///     own universe.  Each cell in the grid is either alive or dead.  An alive cell has an energy above
    ///     the specified threshold.
    ///     This class will be used in each of the milestones.  There are several helper functions that provide
    ///     information about the universe.
    /// </summary>
    public class PlantUniverse
    {
        /// <summary>
        ///     The width of the universe, in terms of cells.
        ///     Any actual "on screen" display is scaled from this.
        /// </summary>
        public const int UniverseWidth = 50;

        /// <summary>
        ///     The height of the universe, in terms of cells.
        ///     Any actual "on screen" display is scaled from this.
        /// </summary>
        public const int UniverseHeight = 100;

        /// <summary>
        ///     The location of the ground line.  Anything >= to this is underground.
        /// </summary>
        public const int GroundLine = UniverseHeight - (UniverseHeight/3);

        /// <summary>
        ///     The size of a cell "info vector".  This vector identifies a cell's state, and is used to encode instructions
        ///     in the genome.  All of these are normalized to [0,1].  There are currently four elements:
        ///     0: The row that the cell is in.
        ///     1: The amount of sunlight the cell is exposed to.
        ///     2: The degree of crowding, from neighbors.
        ///     3: The amount of nourishment the cell is exposed to.
        /// </summary>
        public const int CellVectorLength = 4;

        /// <summary>
        ///     The size of a GENOME vector.  A genome vector is made up of four "info vectors".  These give instructions
        ///     on how to grow a plant.
        ///     Vector 0: Growth template #1
        ///     Vector 1: Growth template #2
        ///     Vector 2: Leaf template
        ///     Vector 3: Stem template
        ///     For more information on how these are used, refer to the PlantGrowth class.
        /// </summary>
        public const int GenomeSize = CellVectorLength*4;

        /// <summary>
        ///     The rate that sunlight decays based on shade, or
        ///     the rate that nourishment decays based on roots absorbing.
        /// </summary>
        public const double Decay = 0.1;

        /// <summary>
        ///     The rate at which leafy material turns to wooden stem.
        /// </summary>
        public const double StemTransition = 0.8;

        /// <summary>
        ///     The threshold to allow growth.
        /// </summary>
        public const double GrowthThreshold = 0.25;


        /// <summary>
        ///     The minimum distance to a genome template to execute that instruction.
        ///     Used to control how growth happens.
        /// </summary>
        public const double MinGrowthDist = 0.9;

        /// <summary>
        ///     The minimum energy that a living cell is allowed to drop to.
        /// </summary>
        public const double MinLivingEnergy = 0.1;

        /// <summary>
        ///     The population size for the genetic algorithm.
        /// </summary>
        public const int PopulationSize = 1000;

        /// <summary>
        ///     How many cycles should we allow the plant to grow for?
        /// </summary>
        public const int EvaluationCycles = 100;

        /// <summary>
        ///     The required root ratio between how leafy the surface is and how nourished the roots are.
        /// </summary>
        public const double RequiredRootRatio = 0.5;

        /// <summary>
        ///     The actual grid, that holds the universe.
        /// </summary>
        private readonly PlantUniverseCell[][] _grid;

        /// <summary>
        ///     Construct the universe and create the grid.
        /// </summary>
        public PlantUniverse()
        {
            _grid = new PlantUniverseCell[UniverseHeight][];

            for (int row = 0; row < _grid.Length; row++)
            {
                _grid[row] = new PlantUniverseCell[UniverseWidth];
                for (int col = 0; col < _grid[row].Length; col++)
                {
                    _grid[row][col] = new PlantUniverseCell();
                }
            }
        }

        /// <summary>
        ///     The amount of nourishment that is held inside of the roots.  This is used to calculate the
        ///     root ratio, that limits growth.
        /// </summary>
        public double RootCount { get; set; }

        /// <summary>
        ///     The amount of leafy material above the surface.  This is used to calculate the root ratio,
        ///     that limits growth.
        /// </summary>
        public double SurfaceCount { get; set; }

        /// <summary>
        ///     Get a cell, using row and column index.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The cell.</returns>
        public PlantUniverseCell GetCell(int row, int col)
        {
            return _grid[row][col];
        }

        /// <summary>
        ///     Calculate the degree of crowding in a cell. Leafy cells
        ///     produce more crowding than stem.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The crowd imposed by this cell.</returns>
        public double CalculateCrowd(int row, int col)
        {
            if (!IsValid(row, col))
                return 0;

            PlantUniverseCell cell = GetCell(row, col);

            if (!cell.IsAlive)
            {
                return 0;
            }

            return cell.Leafyness;
        }


        /// <summary>
        ///     Calculate the degree of crowding around a cell.
        ///     This is the mean crowding of the neighbors.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The mean crowdedness of the cell.</returns>
        public double CalculateMeanNeighborsCrowd(int row, int col)
        {
            double sum = 0;
            sum += CalculateCrowd(row - 1, col - 1);
            sum += CalculateCrowd(row - 1, col);
            sum += CalculateCrowd(row - 1, col + 1);

            sum += CalculateCrowd(row, col - 1);
            sum += CalculateCrowd(row, col + 1);

            sum += CalculateCrowd(row + 1, col - 1);
            sum += CalculateCrowd(row + 1, col);
            sum += CalculateCrowd(row + 1, col + 1);

            return sum/8.0;
        }


        /// <summary>
        ///     Return an info vector about a cell.  This allows cells to be identified by instructions in the genome.
        ///     The vector contains four doubles.  All doubles range from [0,1].
        ///     Element 0: The height of the cell. 1.0 for the last row and 0.0 for the first row.
        ///     Element 1: The amount of sunlight (for surface cells) or water (for underground cells) exposure for this cell.
        ///     Element 2: Crowding by neighbors.
        ///     Element 3: Nourishment for this cell.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The info vector.</returns>
        public double[] GetCellInfoVector(int row, int col)
        {
            var result = new double[CellVectorLength];
            PlantUniverseCell cell = GetCell(row, col);

            // Height
            result[0] = row/UniverseHeight;
            // Sunlight
            if (row < GroundLine)
            {
                result[1] = cell.CalculatedSunlight;
            }
            else
            {
                result[1] = cell.CalculatedWater;
            }
            // Crowd
            result[2] = CalculateMeanNeighborsCrowd(row, col);
            // Nourishment
            result[3] = cell.Nourishment;
            //


            return result;
        }

        /// <summary>
        ///     Reset the entire grid to a single seed.
        /// </summary>
        public void Reset()
        {
            foreach (var aGrid in _grid)
            {
                foreach (PlantUniverseCell cell in aGrid)
                {
                    cell.Leafyness = 0;
                    cell.Energy = 0;
                    cell.Nourishment = 0;
                }
            }

            const int center = UniverseWidth/2;
            const int groundLevel = GroundLine;

            // root
            _grid[groundLevel][center].Leafyness = 0;
            _grid[groundLevel][center].Nourishment = 1;
            _grid[groundLevel][center].Energy = 1;

            // stem
            _grid[groundLevel - 1][center].Leafyness = 0.5;
            _grid[groundLevel - 1][center].Nourishment = 1;
            _grid[groundLevel - 1][center].Energy = 1;

            // leaf
            _grid[groundLevel - 2][center].Leafyness = 1;
            _grid[groundLevel - 2][center].Nourishment = 1;
            _grid[groundLevel - 2][center].Energy = 1;
        }

        /// <summary>
        ///     Returns true if a cell is valid. Invalid cells are off the bounds of a grid.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>True, if valid.</returns>
        public bool IsValid(int row, int col)
        {
            if (row < 0 || col < 0)
            {
                return false;
            }

            if (row >= _grid.Length)
            {
                return false;
            }

            if (col >= _grid[row].Length)
            {
                return false;
            }

            return true;
        }

        /// <summary>
        ///     Calculate the energy for a cell.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The info vector.</returns>
        public double CalculateEnergy(int row, int col)
        {
            if (!IsValid(row, col))
            {
                return 0;
            }
            return _grid[row][col].Energy;
        }

        /// <summary>
        ///     Calculate the transfer energy for a cell.  This is the amount of energy transferred into a cell.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The amount of energy transferred in.</returns>
        public double CalculateTransferEnergy(int row, int col)
        {
            double result = 0;
            result = Math.Max(result, CalculateEnergy(row - 1, col - 1));
            result = Math.Max(result, CalculateEnergy(row - 1, col));
            result = Math.Max(result, CalculateEnergy(row - 1, col + 1));
            return result;
        }

        /// <summary>
        ///     Calculate the transfer nourishment for a cell.  This is the amount of nourishment transferred into a cell.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The amount of energy transferred in.</returns>
        public double CalculateTransferNourishment(int row, int col)
        {
            double result = 0;
            result = Math.Max(result, CalculateEnergy(row + 1, col - 1));
            result = Math.Max(result, CalculateEnergy(row + 1, col));
            result = Math.Max(result, CalculateEnergy(row + 1, col + 1));
            return result;
        }

        /// <summary>
        ///     Count the number of live cells as neighbors to a cell.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>The neighbor count.</returns>
        public int CountNeighbors(int row, int col)
        {
            int sum = 0;

            if (IsAlive(row - 1, col))
            {
                sum++;
            }
            if (IsAlive(row + 1, col))
            {
                sum++;
            }
            if (IsAlive(row, col - 1))
            {
                sum++;
            }
            if (IsAlive(row, col + 1))
            {
                sum++;
            }

            if (IsAlive(row - 1, col - 1))
            {
                sum++;
            }
            if (IsAlive(row + 1, col + 1))
            {
                sum++;
            }
            if (IsAlive(row - 1, col + 1))
            {
                sum++;
            }
            if (IsAlive(row + 1, col - 1))
            {
                sum++;
            }

            return sum;
        }

        /// <summary>
        ///     Returns true, if the specified cell can grow.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>True, if the specified cell is allowed to grow.</returns>
        public bool CanGrow(int row, int col)
        {
            PlantUniverseCell cell = GetCell(row, col);
            if (cell.IsAlive)
            {
                if (row >= GroundLine)
                {
                    return CountNeighbors(row, col) < 4;
                }
                return cell.Energy > GrowthThreshold && cell.Nourishment > GrowthThreshold;
            }
            return false;
        }

        /// <summary>
        ///     Returns true, if the specified cell is alive.
        ///     Alive cells have energy.
        /// </summary>
        /// <param name="row">The row.</param>
        /// <param name="col">The column.</param>
        /// <returns>True, if the specified cell is alive to grow.</returns>
        public bool IsAlive(int row, int col)
        {
            return IsValid(row, col) && (_grid[row][col].IsAlive);
        }
    }
}
