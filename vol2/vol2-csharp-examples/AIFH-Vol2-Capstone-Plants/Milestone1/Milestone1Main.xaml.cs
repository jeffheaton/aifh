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
using System.Windows.Shapes;

namespace AIFH_Vol2_Capstone_Plants.Milestone1
{
    /// <summary>
    /// Interaction logic for Milestone1Main.xaml
    /// </summary>
    public partial class Milestone1Main : Window
    {
        public Milestone1Main()
        {
            InitializeComponent();
        }

        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            DisplayPlant display = new DisplayPlant(CanvasOutput);
            display.Universe = new PlantUniverse();
            display.Universe.Reset();
            display.Paint(CanvasOutput);
        }
    }
}
