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
using AIFH_Vol2_Capstone_Plants.Milestone1;
using AIFH_Vol2_Capstone_Plants.Milestone2;
using AIFH_Vol2_Capstone_Plants.Milestone3;

namespace AIFH_Vol2_Capstone_Plants
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Milestone1Seed_Click(object sender, RoutedEventArgs e)
        {
            Milestone1Main prg = new Milestone1Main();
            prg.Show();
        }

        private void Milestone2Grow_Click(object sender, RoutedEventArgs e)
        {
            Milestone2Main prg = new Milestone2Main();
            prg.Show();
        }

        private void Milestone3Evolve_Click(object sender, RoutedEventArgs e)
        {
            Milestone3Main prg = new Milestone3Main();
            prg.Show();
        }
    }
}
