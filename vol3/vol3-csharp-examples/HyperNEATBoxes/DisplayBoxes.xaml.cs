using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using Encog.MathUtil.Randomize;
using Encog.Neural.NEAT;
using Encog.Neural.NEAT.Training;
using Encog.Neural.HyperNEAT.Substrate;
using Encog.Neural.HyperNEAT;

namespace HyperNEATBoxes
{
    /// <summary>
    /// Interaction logic for DisplayBoxes.xaml
    /// </summary>
    public partial class DisplayBoxes : Window
    {
        private BoxTrialCase testCase = new BoxTrialCase(new Random());
        private NEATPopulation pop;
        private int resolution = BoxTrialCase.BASE_RESOLUTION;

        public DisplayBoxes(NEATPopulation thePopulation)
        {
            InitializeComponent();
            testCase.InitTestCase(0);
            this.pop = thePopulation;
            Render();
        }

        private void NewCase_Click(object sender, RoutedEventArgs e)
        {
            EncogRandom r = new EncogRandom();
            this.resolution = int.Parse(((ComboBoxItem)Resolution.SelectedValue).Content.ToString());
            this.testCase.InitTestCase(r.Next(3));
            Render();
        }

        public void Render()
        {
            NEATGenome genome = (NEATGenome)this.pop.BestGenome;
            Substrate substrate = SubstrateFactory.factorSandwichSubstrate(resolution, resolution);
            HyperNEATCODEC codec = new HyperNEATCODEC();
            NEATNetwork phenotype = (NEATNetwork)codec.Decode(this.pop, substrate, genome);

            TrialEvaluation trial = new TrialEvaluation(phenotype, this.testCase);
            IntPair actualPos = trial.Query(resolution);

            // clear what was there before
            GridCanvas.Children.Clear();

            //
            double boxWidth = GridCanvas.ActualWidth / resolution;
            double boxHeight = GridCanvas.ActualHeight / resolution;
            double delta = 2.0 / resolution;
            int index = 0;

            for (int row = 0; row < resolution; row++)
            {
                double y = -1 + (row * delta);
                double boxY = row * boxHeight;
                for (int col = 0; col < resolution; col++)
                {
                    double x = -1 + (col * delta);
                    double boxX = col * boxWidth;

                    Rectangle r = new Rectangle();
                    r.SetValue(Canvas.LeftProperty, boxX);
                    r.SetValue(Canvas.TopProperty, boxY);
                    r.Width = boxWidth;
                    r.Height = boxHeight;

                    if (this.testCase.GetPixel(x, y) > 0)
                    {
                        r.Fill = Brushes.Blue;
                    }
                    else
                    {
                        double d = trial.Output[index];
                        int c = trial.Normalize(d, 255);
                        SolidColorBrush b = new SolidColorBrush(Color.FromRgb(255, (byte)c, 255));
                        r.Fill = b;
                        r.Stroke = Brushes.Black;
                    }

                    GridCanvas.Children.Add(r);
                    index++;
                }
            }

            Rectangle target = new Rectangle();
            target.SetValue(Canvas.LeftProperty, actualPos.X * boxWidth);
            target.SetValue(Canvas.TopProperty, actualPos.Y * boxHeight);
            target.Width = boxWidth;
            target.Height = boxHeight;
            target.Fill = Brushes.Red;
            GridCanvas.Children.Add(target);
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            Render();
        }
    }
}
