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
using System.Collections.Generic;
using System.ComponentModel;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;
using System.Windows.Threading;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2_MergePhysics.Physics;
using AIFH_Vol2_MergePhysics.Properties;
using AIFH_Vol2_MergePhysics.Universe;
using AIFH_Vol2_MergePhysics.Viewer;
using Microsoft.Win32;

namespace AIFH_Vol2_MergePhysics
{
    /// <summary>
    ///     Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        /// <summary>
        /// The universe cells in the multiverse.
        /// </summary>
        private readonly IList<UniverseDisplayCell> _cells = new List<UniverseDisplayCell>();

        /// <summary>
        /// Random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        /// The background thread.
        /// </summary>
        private Thread _background;


        /// <summary>
        /// The source for a universe copy.
        /// </summary>
        private UniverseRunner _copySource;

        /// <summary>
        /// The first parent for a crossover.
        /// </summary>
        private UniverseRunner _crossoverParent1;

        /// <summary>
        /// The second parent for a crossover.
        /// </summary>
        private UniverseRunner _crossoverParent2;

        /// <summary>
        /// The cells in the multiverse in grid form.
        /// </summary>
        private UniverseDisplayCell[][] _multiverse;

        /// <summary>
        ///     Are the universes running?
        /// </summary>
        private volatile bool _running;

        /// <summary>
        /// The selected cell.
        /// </summary>
        private UniverseDisplayCell _selectedCell;

        /// <summary>
        /// The selected column.
        /// </summary>
        private int _selectedCol;

        /// <summary>
        /// The selected row.
        /// </summary>
        private int _selectedRow;

        /// <summary>
        ///     Has a stop been requested?
        /// </summary>
        private bool _stopRequest;

        public MainWindow()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Setup after the window loads.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            double univWidth = Settings.Default.PaneWidth;
            double univHeight = Settings.Default.PaneHeight;

            CheckAutoKill.IsChecked = true;

            _multiverse = new UniverseDisplayCell[Settings.Default.UniversePaneRows][];
            for (int row = 0; row < Settings.Default.UniversePaneRows; row++)
            {
                _multiverse[row] = new UniverseDisplayCell[Settings.Default.UniversePaneColumns];
                for (int col = 0; col < Settings.Default.UniversePaneColumns; col++)
                {
                    double x = col*univWidth;
                    double y = row*univHeight;

                    // Label
                    var lab = new Label
                    {
                        Foreground = Brushes.Black,
                        Background = Brushes.LightGray,
                        FontSize = 10,
                        Content = "..."
                    };
                    lab.SetValue(Canvas.LeftProperty, x);
                    lab.SetValue(Canvas.TopProperty, y);

                    // Universe image.
                    int paneWidth = Settings.Default.PaneWidth;
                    int paneHeight = Settings.Default.PaneHeight;
                    _multiverse[row][col] = new UniverseDisplayCell(paneWidth, paneHeight, lab);
                    _multiverse[row][col].UniverseRunner.Randomize(new MersenneTwisterGenerateRandom());
                    _multiverse[row][col].Visualize();
                    _multiverse[row][col].UniverseRunner.AutoKill = true;
                    _cells.Add(_multiverse[row][col]);
                    var img = new Image {Source = _multiverse[row][col].Image};
                    img.SetValue(Canvas.LeftProperty, x);
                    img.SetValue(Canvas.TopProperty, y);
                    CanvasOutput.Children.Add(img);

                    // Universe grid.
                    var rect = new Rectangle
                    {
                        //Fill = _currentGrid[row][col] ? Brushes.Black : Brushes.White,
                        Width = univWidth,
                        Height = univHeight,
                        Stroke = Brushes.Black
                    };
                    rect.SetValue(Canvas.LeftProperty, x);
                    rect.SetValue(Canvas.TopProperty, y);
                    CanvasOutput.Children.Add(rect);

                    // Add label
                    CanvasOutput.Children.Add(lab);
                }
            }

            CanvasOutput.SetValue(WidthProperty, Settings.Default.UniversePaneColumns*univWidth);
            CanvasOutput.SetValue(HeightProperty, Settings.Default.UniversePaneRows*univHeight);
            BtnDeselect.IsEnabled = false;
        }

        /// <summary>
        /// The config button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void BtnConfig_Click(object sender, RoutedEventArgs e)
        {
            var dialog = new ConfigDialog();
            dialog.Show();
        }

        /// <summary>
        /// The single step button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void BtnSingleStep_Click(object sender, RoutedEventArgs e)
        {
            Parallel.ForEach(_cells, universe => universe.Advance());

            if (!Dispatcher.CheckAccess())
            {
                Dispatcher.Invoke(UpdateUi);
            }
            else
            {
                UpdateUi();
            }
        }

        /// <summary>
        /// Update the user interface.
        /// </summary>
        private void UpdateUi()
        {
            if (!_stopRequest)
            {
                foreach (UniverseDisplayCell universe in _cells)
                {
                    universe.Visualize();

                    if (_copySource != null)
                    {
                        universe.Caption.Content = universe.UniverseRunner == _selectedCell.UniverseRunner ? "Source" : "";
                    }
                    else if (_crossoverParent1 != null
                             || _crossoverParent2 != null)
                    {
                        if (universe.UniverseRunner == _crossoverParent1)
                        {
                            universe.Caption.Content = "Father";
                        }
                        else if (universe.UniverseRunner == _crossoverParent2)
                        {
                            universe.Caption.Content = "Mother";
                        }
                        else
                        {
                            universe.Caption.Content = "";
                        }
                    }
                    else
                    {
                        String s = "diff: "
                                   + universe.UniverseRunner.Diff + ",age: "
                                   + universe.UniverseRunner.Iterations;
                        universe.Caption.Content = s;
                    }
                }
            }
        }

        /// <summary>
        /// Background thread.
        /// </summary>
        private void DoWork()
        {
            _running = true;

            while (!_stopRequest)
            {
                Parallel.ForEach(_cells, universe => universe.Advance());

                lock (this)
                {
                    if (!Dispatcher.CheckAccess())
                    {
                        Dispatcher.Invoke(UpdateUi);
                    }
                    else
                    {
                        UpdateUi();
                    }
                }
            }
            _stopRequest = false;
            _running = false;
        }

        /// <summary>
        /// The start button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void BtnStart_Click(object sender, RoutedEventArgs e)
        {
            if (!_running)
            {
                _background = new Thread(DoWork);
                _background.Start();
            }
        }


        /// <summary>
        /// The stop button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void BtnStop_Click(object sender, RoutedEventArgs e)
        {
            StopAnimation();
        }

        /// <summary>
        /// The reset button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void BtnReset_Click(object sender, RoutedEventArgs e)
        {
            lock (this)
            {
                foreach (UniverseDisplayCell universe in _cells)
                {
                    universe.UniverseRunner.Randomize(_rnd);
                }

                if (!_running)
                {
                    UpdateUi();
                }
            }
        }

        /// <summary>
        /// The deselect button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void BtnDeselect_Click(object sender, RoutedEventArgs e)
        {
            _copySource = null;
            _crossoverParent1 = _crossoverParent2 = null;
            BtnDeselect.IsEnabled = false;
        }

        /// <summary>
        /// Called to stop the animation.
        /// </summary>
        private void StopAnimation()
        {
            if (_running)
            {
                for (;;)
                {
                    _stopRequest = true;
                    if (_background.Join(100)) // set appropriate timeout
                    {
                        break;
                    }

                    // resolve deadlock
                    Application.Current.Dispatcher.Invoke(DispatcherPriority.Background,
                        new Action(delegate { }));
                }
            }
        }

        /// <summary>
        /// The window is closing.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void Window_Closing_1(object sender, CancelEventArgs e)
        {
            StopAnimation();
        }

        /// <summary>
        /// The auto kill button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void CheckAutoKill_Click(object sender, RoutedEventArgs e)
        {
            foreach (UniverseDisplayCell universe in _cells)
            {
                universe.UniverseRunner.AutoKill = CheckAutoKill.IsChecked == true;
            }
        }

        /// <summary>
        /// The save popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuSavePhysics_Click(object sender, RoutedEventArgs e)
        {
            var dlg = new SaveFileDialog {Filter = "All files (*.*)|*.*", FilterIndex = 2, RestoreDirectory = true};

            if (dlg.ShowDialog() == true)
            {
                _selectedCell.UniverseRunner.PhysicsRules.Save(dlg.FileName);
            }
        }

        /// <summary>
        /// The load popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuLoadPhysics_Click(object sender, RoutedEventArgs e)
        {
            var dlg = new OpenFileDialog {Filter = "All files (*.*)|*.*", FilterIndex = 2, RestoreDirectory = true};

            if (dlg.ShowDialog() == true)
            {
                _selectedCell.UniverseRunner.PhysicsRules.Load(dlg.FileName);
            }
        }

        /// <summary>
        /// The kill universe popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuKillUniverse_Checked(object sender, RoutedEventArgs e)
        {
            _selectedCell.UniverseRunner.Reset(_rnd);
        }

        /// <summary>
        /// The big bang popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuBigBang_Click(object sender, RoutedEventArgs e)
        {
            _selectedCell.UniverseRunner.Randomize(_rnd);
        }

        /// <summary>
        /// The mutate across popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuMutateAcross_Click(object sender, RoutedEventArgs e)
        {
            int rows = Settings.Default.UniversePaneRows;
            int cols = Settings.Default.UniversePaneColumns;

            IPhysics sourcePhysics = _selectedCell.UniverseRunner.PhysicsRules;
            for (int currentRow = 0; currentRow < rows; currentRow++)
            {
                for (int currentCol = 0; currentCol < cols; currentCol++)
                {
                    if (currentRow != _selectedRow || currentCol != _selectedCol)
                    {
                        _multiverse[currentRow][currentCol].UniverseRunner
                            .Mutate(_rnd, sourcePhysics, 0.5, 0.2);
                        _multiverse[currentRow][currentCol].UniverseRunner
                            .Randomize(_rnd);
                    }
                }
            }
        }

        /// <summary>
        /// The mutate single popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuMutateSingle_Click(object sender, RoutedEventArgs e)
        {
            UniverseRunner target = _selectedCell.UniverseRunner;
            target.Mutate(_rnd, target.PhysicsRules, 0.5, 0.2);
            target.Randomize(_rnd);
        }

        /// <summary>
        /// The crossover popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuCrossover_Click(object sender, RoutedEventArgs e)
        {
            _crossoverParent1 = _selectedCell.UniverseRunner;
            _crossoverParent2 = null;
            _copySource = null;
            BtnDeselect.IsEnabled = true;
        }

        /// <summary>
        /// The copy popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuCopyPane_Click(object sender, RoutedEventArgs e)
        {
            _copySource = _selectedCell.UniverseRunner;
            _crossoverParent1 = null;
            _crossoverParent2 = null;
            BtnDeselect.IsEnabled = true;
        }

        /// <summary>
        /// The run singular popup menu was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void MenuRunSingular_Click(object sender, RoutedEventArgs e)
        {
            StopAnimation();
            var window = new DisplaySingle(_selectedCell.UniverseRunner);
            window.Show();
        }

        /// <summary>
        /// The mouse was pressed.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void CanvasOutput_MouseDown(object sender, MouseButtonEventArgs e)
        {
            // update selected cell
            Point pt = Mouse.GetPosition(CanvasOutput);

            int univWidth = Settings.Default.PaneWidth;
            int univHeight = Settings.Default.PaneHeight;

            _selectedRow = (int) (pt.Y/univWidth);
            _selectedCol = (int) (pt.X/univHeight);

            _selectedCell = _multiverse[_selectedRow][_selectedCol];

            // if left-click then perform correct operation (if any)
            if (e.LeftButton == MouseButtonState.Pressed)
            {
                if (_copySource != null)
                {
                    UniverseRunner target = _selectedCell
                        .UniverseRunner;
                    target.PhysicsRules.CopyData(
                        _copySource.PhysicsRules.Data);
                    target.Randomize(_rnd);
                }
                else if (_crossoverParent1 != null
                         && _crossoverParent2 == null)
                {
                    _crossoverParent2 = _selectedCell.UniverseRunner;
                }
                else
                {
                    UniverseRunner target = _selectedCell.UniverseRunner;
                    target.Crossover(_rnd, _crossoverParent1, _crossoverParent2);
                    target.Randomize(_rnd);
                }
            }
        }
    }
}
