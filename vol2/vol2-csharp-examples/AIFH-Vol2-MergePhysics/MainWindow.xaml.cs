using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using AIFH_Vol2_MergePhysics.Viewer;

namespace AIFH_Vol2_MergePhysics
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private UniverseDisplayCell[][] _multiverse;

        public MainWindow()
        {
            InitializeComponent();            
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            double univWidth = Properties.Settings.Default.PaneWidth;
            double univHeight = Properties.Settings.Default.PaneHeight;

            _multiverse = new UniverseDisplayCell[Properties.Settings.Default.UniversePaneRows][];
            for (int row = 0; row < Properties.Settings.Default.UniversePaneRows; row++)
            {
                _multiverse[row] = new UniverseDisplayCell[Properties.Settings.Default.UniversePaneColumns];
                for (int col = 0; col < Properties.Settings.Default.UniversePaneColumns; col++)
                {
                    var rect = new Rectangle
                    {
                        //Fill = _currentGrid[row][col] ? Brushes.Black : Brushes.White,
                        Width = univWidth,
                        Height = univHeight,
                        Stroke = Brushes.Black
                    };
                    rect.SetValue(Canvas.LeftProperty, col*univWidth);
                    rect.SetValue(Canvas.TopProperty, row*univHeight);
                    CanvasOutput.Children.Add(rect);
                }
            }

            CanvasOutput.SetValue(Canvas.WidthProperty,Properties.Settings.Default.UniversePaneColumns*univWidth);
            CanvasOutput.SetValue(Canvas.HeightProperty,Properties.Settings.Default.UniversePaneRows*univHeight);
        }
    }
}
