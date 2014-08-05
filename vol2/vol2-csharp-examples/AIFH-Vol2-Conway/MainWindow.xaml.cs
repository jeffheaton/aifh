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
using System.ComponentModel;
using System.Threading;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2_Conway
{
    /// <summary>
    ///     Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        public const int CellSize = 6;

        /// <summary>
        ///     Used to locate the x coordinate of neighbors.
        /// </summary>
        private readonly int[] _neighborsX = {0, 0, 1, -1, -1, 1, -1, 1};

        /// <summary>
        ///     Used to locate the y coordinate of neighbors.
        /// </summary>
        private readonly int[] _neighborsY = {1, -1, 0, 0, -1, -1, 1, 1};

        /// <summary>
        ///     The current state of the grid.
        /// </summary>
        private bool[][] _currentGrid;

        /// <summary>
        ///     The grid displayed as XAML objects.
        /// </summary>
        private Rectangle[][] _grid;

        /// <summary>
        ///     The next grid to display.
        /// </summary>
        private bool[][] _nextGrid;

        /// <summary>
        ///     Stop the thread.
        /// </summary>
        private volatile bool _shouldStop;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            ButtonStop.IsEnabled = false;
            Reset();
        }

        private void ButtonReset_Click(object sender, RoutedEventArgs e)
        {
            Reset();
        }

        private void Reset()
        {
            var rows = (int) (CanvasOutput.ActualHeight/CellSize);
            var columns = (int) (CanvasOutput.ActualWidth/CellSize);
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            CanvasOutput.Children.Clear();

            _grid = new Rectangle[rows][];
            _nextGrid = new bool[rows][];
            _currentGrid = new bool[rows][];

            for (int row = 0; row < rows; row++)
            {
                _grid[row] = new Rectangle[columns];
                _nextGrid[row] = new bool[columns];
                _currentGrid[row] = new bool[columns];

                for (int col = 0; col < columns; col++)
                {
                    double x = col*CellSize;
                    double y = row*CellSize;

                    _currentGrid[row][col] = rnd.NextBoolean();

                    var rect = new Rectangle
                    {
                        Fill = _currentGrid[row][col] ? Brushes.Black : Brushes.White,
                        Width = CellSize,
                        Height = CellSize,
                        Stroke = Brushes.White
                    };
                    rect.SetValue(Canvas.LeftProperty, x);
                    rect.SetValue(Canvas.TopProperty, y);
                    CanvasOutput.Children.Add(rect);
                    _grid[row][col] = rect;
                }
            }
        }

        public void DoWork()
        {
            while (!_shouldStop)
            {
                Thread.Sleep(100);
                if (!Dispatcher.CheckAccess())
                {
                    Dispatcher.Invoke(LifeStep);
                }
                else
                {
                    LifeStep();
                }
            }
        }

        private void LifeStep()
        {
            for (int row = 0; row < _nextGrid.Length; row++)
            {
                for (int col = 0; col < _nextGrid[row].Length; col++)
                {
                    int total = 0;

                    for (int i = 0; i < _neighborsX.Length; i++)
                    {
                        int nCol = col + _neighborsX[i];
                        int nRow = row + _neighborsY[i];
                        if (nCol >= 0 && nCol < _nextGrid[0].Length)
                        {
                            if (nRow >= 0 && nRow < _nextGrid.Length)
                            {
                                if (_currentGrid[nRow][nCol])
                                {
                                    total++;
                                }
                            }
                        }
                    }


                    bool alive = _currentGrid[row][col];

                    if (alive)
                    {
                        // 1. Any live cell with fewer than two live neighbors dies, as if caused by under-population.
                        if (total < 2)
                        {
                            alive = false;
                        }
                        // 2. Any live cell with two or three live neighbors lives on to the next generation. (not needed)
                        // 3. Any live cell with more than three live neighbors dies, as if by overcrowding.
                        if (alive && total > 3)
                        {
                            alive = false;
                        }
                    }
                    else
                    {
                        // 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
                        if (total == 3)
                        {
                            alive = true;
                        }
                    }

                    _nextGrid[row][col] = alive;
                }
            }

            for (int row = 0; row < _nextGrid.Length; row++)
            {
                for (int col = 0; col < _nextGrid[row].Length; col++)
                {
                    _currentGrid[row][col] = _nextGrid[row][col];
                    _grid[row][col].Fill = _currentGrid[row][col] ? Brushes.Black : Brushes.White;
                }
            }
        }

        private void ButtonStart_Click(object sender, RoutedEventArgs e)
        {
            ButtonStart.IsEnabled = false;
            ButtonReset.IsEnabled = false;
            ButtonStop.IsEnabled = true;
            _shouldStop = false;
            var t = new Thread(DoWork);
            t.Start();
        }

        private void Window_Closing(object sender, CancelEventArgs e)
        {
            _shouldStop = true;
        }

        private void ButtonStop_Click(object sender, RoutedEventArgs e)
        {
            _shouldStop = true;
            ButtonStart.IsEnabled = true;
            ButtonReset.IsEnabled = true;
            ButtonStop.IsEnabled = false;
        }
    }
}
