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
using System.Threading;
using System.Windows;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Genetic.Crossover;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Genetic.Mutate;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2_Capstone_Plants.Milestone1;
using AIFH_Vol2_Capstone_Plants.Milestone2;
using AIFH_Vol2.Core.Genetic.Species;

namespace AIFH_Vol2_Capstone_Plants.Milestone3
{
    /// <summary>
    /// Interaction logic for Milestone3Main.xaml
    /// </summary>
    public partial class Milestone3Main : Window
    {
        public Milestone3Main()
        {
            InitializeComponent();
        }

        /**
     * The plant display.
     */
        private DisplayPlant display;

        /**
         * The universe.
         */
        private PlantUniverse universe;

        /**
         * The population.
         */
        private IPopulation pop;

        /**
         * The score function.
         */
        private PlantScore score;

        /**
         * The genetic training.
         */
        private BasicEA genetic;

        /**
         * Random number generator.
         */
        private MersenneTwisterGenerateRandom rnd = new MersenneTwisterGenerateRandom();

        private volatile bool _done;

        /**
         * @return A random genome.
         */
        private DoubleArrayGenome RandomGenome()
        {
            DoubleArrayGenome genome = new DoubleArrayGenome(PlantUniverse.GenomeSize);

            for (int i = 0; i < genome.Count; i++)
            {
                genome.Data[i] = rnd.NextDouble(0, 1);
            }
            return genome;
        }

        /**
         * Create the initial random population.
         *
         * @return The population.
         */
        private IPopulation InitPopulation() {
        IPopulation result = new BasicPopulation(PlantUniverse.PopulationSize, null);

        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.Population = result;
        for (int i = 0; i < PlantUniverse.PopulationSize; i++) {
            DoubleArrayGenome genome = RandomGenome();
            defaultSpecies.Add(genome);
        }
        result.GenomeFactory = new DoubleArrayGenomeFactory(PlantUniverse.GenomeSize);
        result.Species.Add(defaultSpecies);

        return result;
    }

        private void DoWork()
        {
            int generation = 0;

            UpdateStatus("Starting up...");

            while (!_done)
            {
            generation++;
            this.genetic.Iteration();

            this.universe.Reset();

            DoubleArrayGenome bestGenome = (DoubleArrayGenome) this.genetic.BestGenome;
            PlantGrowth growth = new PlantGrowth();
            PlantPhysics physics = new PlantPhysics();

            for (int i = 0; i < PlantUniverse.EvaluationCycles; i++) {
                physics.RunPhysics(universe);
                growth.RunGrowth(universe, bestGenome.Data);
            }

                double bestScore = this.genetic.BestGenome.Score;
                UpdateStatus("Generation: " + generation + ", Best Score: " + bestScore);

            

            //System.out.println(Arrays.toString(bestGenome.getLongTermMemory()));    
            }
        }

        private void UpdateStatus(string text)
        {
            if (!Dispatcher.CheckAccess())
            {
                Dispatcher.Invoke(() => LabelStatus.Content = text);
            }
            else
            {
                LabelStatus.Content = text;
            }
        }

        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            pop = InitPopulation();
            score = new PlantScore();
            this.genetic = new BasicEA(pop, score);

            //this.genetic.Speciation = new ArraySpeciation<DoubleArrayGenome>();

            genetic.AddOperation(0.9, new Splice(PlantUniverse.GenomeSize / 3));
            genetic.AddOperation(0.1, new MutatePerturb(0.1));

            // Display

            this.universe = new PlantUniverse();
            this.universe.Reset();


            DoubleArrayGenome bestGenome = (DoubleArrayGenome)genetic.BestGenome;
            PlantPhysics physics = new PlantPhysics();
            PlantGrowth growth = new PlantGrowth();

            for (int i = 0; i < 100; i++)
            {
                physics.RunPhysics(universe);
                growth.RunGrowth(universe, bestGenome.Data);
            }

            this.display = new DisplayPlant(CanvasOutput);
            this.display.Universe = this.universe;
            Thread t = new Thread(DoWork);
            t.Start();
        }

        private void Window_Closing_1(object sender, System.ComponentModel.CancelEventArgs e)
        {
            _done = true;
        }
    }
}
