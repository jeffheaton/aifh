using System.Windows;
using AIFH_Vol2_Capstone_Plants.Milestone1;
using AIFH_Vol2_Capstone_Plants.Milestone2;
using AIFH_Vol2_Capstone_Plants.Milestone3;

namespace AIFH_Vol2_Capstone_Plants
{
    /// <summary>
    ///     Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Milestone1Seed_Click(object sender, RoutedEventArgs e)
        {
            var prg = new Milestone1Main();
            prg.Show();
        }

        private void Milestone2Grow_Click(object sender, RoutedEventArgs e)
        {
            var prg = new Milestone2Main();
            prg.Show();
        }

        private void Milestone3Evolve_Click(object sender, RoutedEventArgs e)
        {
            var prg = new Milestone3Main();
            prg.Show();
        }
    }
}