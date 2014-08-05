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
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace AIFH_Vol2_ECA
{
    /// <summary>
    ///     Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        public const double BoxWidth = 7.0;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void ButtonGenerate_Click(object sender, RoutedEventArgs e)
        {
            var columns = (int) (CanvasOutput.ActualWidth/BoxWidth);
            var rows = (int) (CanvasOutput.ActualHeight/BoxWidth);
            var grid = new bool[2][];
            int currentBuffer = 0;
            int prevBuffer = 1;

            int rule;

            try
            {
                rule = int.Parse(TextRule.Text);
            }
            catch (FormatException)
            {
                rule = -1;
            }

            if (rule < 0 || rule > 255)
            {
                MessageBox.Show("Please enter a valid rule (0-255).");
                return;
            }

            // Decode the rule.
            var output = new bool[8];
            int cx = 1;
            int idx = 7;
            while (idx > 0)
            {
                output[idx--] = (rule & cx) != 0;
                cx *= 2;
            }

            // Create a buffer for current row and previous.
            for (int i = 0; i < grid.Length; i++)
            {
                grid[i] = new bool[columns];
            }

            // Seed.
            grid[prevBuffer][columns/2] = true;

            // Render the ECA
            CanvasOutput.Children.Clear();

            for (int row = 0; row < rows; row++)
            {
                for (int col = 0; col < columns; col++)
                {
                    double x = col*BoxWidth;
                    double y = row*BoxWidth;


                    bool result = false;
                    bool a = col > 0 && grid[prevBuffer][col - 1];
                    bool b = grid[prevBuffer][col];
                    bool c = col < grid[prevBuffer].Length - 1 && grid[prevBuffer][col + 1];

                    if (a && b && c)
                    {
                        result = output[0];
                    }
                    else if (a && b && !c)
                    {
                        result = output[1];
                    }
                    else if (a && !b && c)
                    {
                        result = output[2];
                    }
                    else if (a && !b && !c)
                    {
                        result = output[3];
                    }
                    else if (!a && b && c)
                    {
                        result = output[4];
                    }
                    else if (!a && b && !c)
                    {
                        result = output[5];
                    }
                    else if (!a && !b && c)
                    {
                        result = output[6];
                    }
                    else if (!a && !b && !c)
                    {
                        result = output[7];
                    }

                    var rect = new Rectangle
                    {
                        Fill = result ? Brushes.Black : Brushes.White,
                        Width = BoxWidth,
                        Height = BoxWidth,
                        Stroke = result ? Brushes.Black : Brushes.White
                    };
                    rect.SetValue(Canvas.LeftProperty, x);
                    rect.SetValue(Canvas.TopProperty, y);
                    CanvasOutput.Children.Add(rect);
                    grid[currentBuffer][col] = result;
                }

                // Swap buffers.
                int temp = prevBuffer;
                prevBuffer = currentBuffer;
                currentBuffer = temp;
            }
        }
    }
}
