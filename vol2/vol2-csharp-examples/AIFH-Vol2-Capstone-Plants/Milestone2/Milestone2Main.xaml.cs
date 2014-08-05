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
using System.ComponentModel;
using System.Threading;
using System.Windows;
using AIFH_Vol2_Capstone_Plants.Milestone1;

namespace AIFH_Vol2_Capstone_Plants.Milestone2
{
    /// <summary>
    ///     Interaction logic for Milestone2Main.xaml
    /// </summary>
    public partial class Milestone2Main
    {
        public readonly double[] SamplePlant =
        {
            0.08414097456375995, 0.11845586131703176, 0.1868971940834313, 0.4346911204161327,
            0.024190631402031804, 0.5773526701833149, 0.8997253827355136, 0.9267311086327318, 0.04639229538493471,
            0.8190692654645835,
            0.06531672676605614, 0.026431639742068264, 0.31497914852215286, 1.0276526539348398, 0.03303133293309127,
            0.35946010922382937
        };

        private DisplayPlant _display;
        private bool _done;

        public Milestone2Main()
        {
            InitializeComponent();
        }

        private void Init()
        {
            _display = new DisplayPlant(CanvasOutput);
        }

        public void DoWork()
        {
            var physics = new PlantPhysics();
            var growth = new PlantGrowth();
            var universe = new PlantUniverse();

            universe.Reset();

            _display.Universe = universe;

            for (int i = 0; i < PlantUniverse.EvaluationCycles && !_done; i++)
            {
                physics.RunPhysics(universe);
                growth.RunGrowth(universe, SamplePlant);

                if (!Dispatcher.CheckAccess())
                {
                    Dispatcher.Invoke(() => _display.Paint());
                }
                else
                {
                    _display.Paint();
                }

                Thread.Sleep(100);
            }
            Thread.Sleep(100);
        }

        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            if (!Dispatcher.CheckAccess())
            {
                Dispatcher.Invoke(Init);
            }
            else
            {
                Init();
            }


            var t = new Thread(DoWork);
            t.Start();
        }

        private void Window_Closing_1(object sender, CancelEventArgs e)
        {
            _done = true;
        }
    }
}
