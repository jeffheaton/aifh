using System;
using System.Windows;
using AIFH_Vol2_MergePhysics.Properties;

namespace AIFH_Vol2_MergePhysics.Viewer
{
    /// <summary>
    ///     Interaction logic for ConfigDialog.xaml
    /// </summary>
    public partial class ConfigDialog : Window
    {
        public ConfigDialog()
        {
            InitializeComponent();
        }

        private void InitDisplay()
        {
            TextZoom.Text = "" + Settings.Default.Zoom;
            TextPaneColumns.Text = "" + Settings.Default.UniversePaneColumns;
            TextPaneRows.Text = "" + Settings.Default.UniversePaneRows;
            TextPaneWidth.Text = "" + Settings.Default.PaneWidth;
            TextPaneHeight.Text = "" + Settings.Default.PaneHeight;
        }

        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            InitDisplay();
        }

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

        private void ButtonDefaults_Click(object sender, RoutedEventArgs e)
        {
            Settings.Default.Reset();
        }

        private void ButtonCancel_Click(object sender, RoutedEventArgs e)
        {
            Close();
        }
    }
}