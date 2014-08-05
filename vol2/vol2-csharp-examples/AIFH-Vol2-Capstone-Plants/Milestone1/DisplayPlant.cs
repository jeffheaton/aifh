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
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace AIFH_Vol2_Capstone_Plants.Milestone1
{
    /// <summary>
    ///     A class used to display a plant. This class will be used in all milestones.
    /// </summary>
    public class DisplayPlant
    {
        /// <summary>
        ///     The color of the dirt.
        /// </summary>
        private readonly Brush _dirtColor = new SolidColorBrush(Color.FromArgb(255, 96, 96, 96));

        /// <summary>
        ///     The most brown color that a plant can take on.
        /// </summary>
        private readonly SolidColorBrush _fullBrown = new SolidColorBrush(Color.FromArgb(255, 165, 42, 42));

        /// <summary>
        ///     The most green color that a plant can take on.
        /// </summary>
        private readonly SolidColorBrush _fullGreen =
            new SolidColorBrush(Color.FromArgb(255, 0, 255, 0));

        /// <summary>
        ///     The color of the sky.
        /// </summary>
        private readonly Brush _skyColor = new SolidColorBrush(Color.FromArgb(255, 135, 206, 250));

        /// <summary>
        ///     The display grid.
        /// </summary>
        private readonly Rectangle[][] _grid;

        /// <summary>
        ///     A gradient of colors between full green and brown.
        /// </summary>
        private readonly Brush[] _gradient;


        /// <summary>
        ///     Constructor.
        /// </summary>
        /// <param name="canvas">The canvas to render to.</param>
        public DisplayPlant(Canvas canvas)
        {
            int gradentRangeRed = _fullGreen.Color.R - _fullBrown.Color.R;
            int gradentRangeGreen = _fullGreen.Color.G - _fullBrown.Color.G;
            int gradentRangeBlue = _fullGreen.Color.B - _fullBrown.Color.B;

            int maxRange = Math.Max(Math.Max(
                Math.Abs(gradentRangeRed), Math.Abs(gradentRangeGreen)), Math.Abs(gradentRangeBlue));

            double scaleRed = gradentRangeRed/(double) maxRange;
            double scaleGreen = gradentRangeGreen/(double) maxRange;
            double scaleBlue = gradentRangeBlue/(double) maxRange;

            _gradient = new Brush[maxRange];

            for (int i = 0; i < maxRange; i++)
            {
                _gradient[i] = new SolidColorBrush(Color.FromArgb(255,
                    (byte) (_fullBrown.Color.R + (i*scaleRed)),
                    (byte) (_fullBrown.Color.G + (i*scaleGreen)),
                    (byte) (_fullBrown.Color.B + (i*scaleBlue))
                    ));
            }


            double width = canvas.ActualWidth;
            double height = canvas.ActualHeight;

            // Universe always has a fixed number of cells.  Determine the on-screen dimensions of a cell.
            double cellWidth = Math.Max(width/PlantUniverse.UniverseWidth, 1);
            double cellHeight = Math.Max(height/PlantUniverse.UniverseHeight, 1);

            _grid = new Rectangle[PlantUniverse.UniverseHeight][];

            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                _grid[row] = new Rectangle[PlantUniverse.UniverseWidth];
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    var rect = new Rectangle
                    {
                        Fill = Brushes.Black,
                        Width = cellWidth + 1,
                        Height = cellHeight + 1
                    };
                    rect.SetValue(Canvas.LeftProperty, col*cellWidth);
                    rect.SetValue(Canvas.TopProperty, row*cellHeight);
                    canvas.Children.Add(rect);
                    _grid[row][col] = rect;
                }
            }
        }

        /// <summary>
        ///     The universe that we will render from.
        /// </summary>
        public PlantUniverse Universe { get; set; }

        /// <summary>
        ///     Paint the plant.
        /// </summary>
        /// <returns></returns>
        public void Paint()
        {
            // Loop over all cells.
            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    PlantUniverseCell cell = Universe.GetCell(row, col);

                    Brush brush;
                    // For empty cells display either the ground or sky color.
                    // Roots are always white.
                    if (row >= PlantUniverse.GroundLine)
                    {
                        brush = cell.IsAlive ? Brushes.White : _dirtColor;
                    }
                    else
                    {
                        if (cell.IsAlive)
                        {
                            var idx = (int) ((_gradient.Length - 1)*cell.Leafyness);
                            brush = _gradient[idx];
                        }
                        else
                        {
                            brush = _skyColor;
                        }
                    }

                    _grid[row][col].Fill = brush;
                }
            }
        }
    }
}
