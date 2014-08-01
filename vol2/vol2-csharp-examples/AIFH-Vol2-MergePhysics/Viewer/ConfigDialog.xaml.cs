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