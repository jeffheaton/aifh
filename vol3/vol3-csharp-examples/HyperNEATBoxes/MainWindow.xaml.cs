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
using System.Windows.Navigation;
using System.Windows.Shapes;
using Encog.Neural.HyperNEAT.Substrate;
using Encog.Neural.NEAT;
using Encog.ML.EA.Train;
using Encog.Neural.NEAT.Training.Species;
using Encog.Util;
using System.Threading;

namespace HyperNEATBoxes
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private bool trainingUnderway = false;
        private bool requestStop = false;
        private NEATPopulation pop;
        private IEvolutionaryAlgorithm train;
        private bool exampleReady = false;

        delegate void DelegateUpdateStatus();                 

        public MainWindow()
        {
            InitializeComponent();
        }

        private void MenuItem_Click(object sender, RoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        private void Training_Click(object sender, RoutedEventArgs e)
        {
            if (this.trainingUnderway)
            {
                this.Training.IsEnabled = false;
                this.requestStop = true;
            }
            else
            {
                Thread t = new Thread(BackgroundTask);
                t.Start();
            }            
        }

        public void UpdateStatus()
        {
            if( this.trainingUnderway ) {
                this.Example.IsEnabled = false;
                this.Training.Content = "Stop Training";

                if (!double.IsNaN(train.Error) && !double.IsInfinity(train.Error))
                {
                    this.LabelError.Content = Format.FormatDouble(train.Error, 2);
                }
                this.LabelIterationCount.Content = Format.FormatInteger(this.train.IterationNumber);
                this.LabelSpeciesCount.Content = Format.FormatInteger(this.pop.Species.Count);
            } else {
                this.Example.IsEnabled = exampleReady;
                this.Training.Content = "Begin Training";
            }

            
        }

        public void ResetTraining()
        {
            Substrate substrate = SubstrateFactory.factorSandwichSubstrate(11, 11);
            BoxesScore score = new BoxesScore(11);
            pop = new NEATPopulation(substrate, 500);
            pop.ActivationCycles = 4;
            pop.Reset();
            train = NEATUtil.ConstructNEATTrainer(pop, score);
            OriginalNEATSpeciation speciation = new OriginalNEATSpeciation();
            train.Speciation = new OriginalNEATSpeciation();
        }

        public void BackgroundTask()
        {
            ResetTraining();
            this.trainingUnderway = true;
            this.exampleReady = false;
            this.Dispatcher.Invoke(new DelegateUpdateStatus(UpdateStatus), new object[] { });
            this.requestStop = false;

            while (!this.requestStop && this.train.Error < 110)
            {
                this.train.Iteration();
                this.Dispatcher.Invoke(new DelegateUpdateStatus(UpdateStatus), new object[] { });
            }

            this.train.FinishTraining();

            this.exampleReady = true;
            this.trainingUnderway = false;
            this.Dispatcher.Invoke(new DelegateUpdateStatus(UpdateStatus), new object[] { });
        }

        private void Example_Click(object sender, RoutedEventArgs e)
        {
            DisplayBoxes boxes = new DisplayBoxes(this.pop);
            boxes.Show();
        }
    }
}
