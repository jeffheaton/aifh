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
    public partial class MainWindow : Window
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
            catch (FormatException ex)
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

                    var rect = new Rectangle();
                    rect.Fill = result ? Brushes.Black : Brushes.White;
                    rect.Width = BoxWidth;
                    rect.Height = BoxWidth;
                    rect.Stroke = result ? Brushes.Black : Brushes.White;
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