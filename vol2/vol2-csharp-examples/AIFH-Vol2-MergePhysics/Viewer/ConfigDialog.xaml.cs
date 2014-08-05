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
using AIFH_Vol2_MergePhysics.Properties;

namespace AIFH_Vol2_MergePhysics.Viewer
{
    /// <summary>
    ///     Interaction logic for ConfigDialog.xaml
    /// </summary>
    public partial class ConfigDialog
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        public ConfigDialog()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Copy text to display.
        /// </summary>
        private void InitDisplay()
        {
            TextZoom.Text = "" + Settings.Default.Zoom;
            TextPaneColumns.Text = "" + Settings.Default.UniversePaneColumns;
            TextPaneRows.Text = "" + Settings.Default.UniversePaneRows;
            TextPaneWidth.Text = "" + Settings.Default.PaneWidth;
            TextPaneHeight.Text = "" + Settings.Default.PaneHeight;
        }

        /// <summary>
        /// Load the window.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            InitDisplay();
        }

        /// <summary>
        /// The okay button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void ButtonOK_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                Settings.Default.Zoom = int.Parse(TextZoom.Text);
                Settings.Default.UniversePaneColumns = int.Parse(TextPaneColumns.Text);
                Settings.Default.UniversePaneRows = int.Parse(TextPaneRows.Text);
                Settings.Default.PaneWidth = int.Parse(TextPaneWidth.Text);
                Settings.Default.PaneHeight = int.Parse(TextPaneHeight.Text);
                Settings.Default.Save();
                MessageBox.Show("Your new settings will take effect once the application is restarted.");
                Close();
            }
            catch (FormatException)
            {
                MessageBox.Show("Please enter only valid numbers.");
            }
        }

        /// <summary>
        /// The defaults button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void ButtonDefaults_Click(object sender, RoutedEventArgs e)
        {
            Settings.Default.Reset();
        }

        /// <summary>
        /// The cancel button was clicked.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void ButtonCancel_Click(object sender, RoutedEventArgs e)
        {
            Close();
        }
    }
}
