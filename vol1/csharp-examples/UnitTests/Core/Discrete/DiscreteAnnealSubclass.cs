// Artificial Intelligence for Humans
// Volume 1: Fundamental Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2013 by Jeff Heaton
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

using System;
using AIFH_Vol1.Core.Discrete;
using AIFH_Vol1.Core.Distance;
using AIFH_Vol1.Core.Randomize;

namespace UnitTests.Core.Discrete
{
    /// <summary>
    /// Test the discrete anneal subclass.
    /// </summary>
    public class DiscreteAnnealSubclass : DiscreteAnneal
    {
        public static readonly double[] Ideal = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
        private readonly double[] _backupHolder = new double[10];
        private readonly double[] _bestHolder = new double[10];
        private readonly double[] _currentHolder = {10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0};
        private readonly ICalculateDistance _distance = new EuclideanDistance();
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom(1);

        public DiscreteAnnealSubclass(int theKMax, double theStartingTemperature, double theEndingTemperature)
            : base(theKMax, theStartingTemperature, theEndingTemperature)
        {
            Cycles = 1000;
        }

        public double[] Best
        {
            get { return _bestHolder; }
        }

        public override void BackupState()
        {
            Array.Copy(_currentHolder, 0, _backupHolder, 0, _currentHolder.Length);
        }

        public override void RestoreState()
        {
            Array.Copy(_backupHolder, 0, _currentHolder, 0, _currentHolder.Length);
        }

        public override void FoundNewBest()
        {
            Array.Copy(_currentHolder, 0, _bestHolder, 0, _currentHolder.Length);
        }

        public override void MoveToNeighbor()
        {
            // pick the first point to swap
            int pt1 = _rnd.NextInt(_currentHolder.Length);

            // pick the second point to swap, can't be the same as the first
            int pt2;

            do
            {
                pt2 = _rnd.NextInt(_currentHolder.Length);
            } while (pt1 == pt2);

            // swap them
            double temp = _currentHolder[pt1];
            _currentHolder[pt1] = _currentHolder[pt2];
            _currentHolder[pt2] = temp;
        }

        public override double Evaluate()
        {
            return _distance.Calculate(Ideal, _currentHolder);
        }
    }
}