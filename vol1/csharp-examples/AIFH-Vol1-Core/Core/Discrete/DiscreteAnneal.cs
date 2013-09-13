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
using System.Text;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Core.Discrete
{
    /// <summary>
    /// Perform discrete simulated annealing.  Discrete simulated annealing involves a problem with
    /// a finite number of positions (or potential solutions).
    /// </summary>
    public abstract class DiscreteAnneal
    {
        /// <summary>
        /// The random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        /// The current global best score.  The global best score is the best score that has been found 
        /// over all of the iterations.
        /// </summary>
        private double _globalBestScore = double.PositiveInfinity;

        /// <summary>
        /// The current score.
        /// </summary>
        private double _currentScore;

        /// <summary>
        /// The maximum number of iterations to try.
        /// </summary>
        private readonly int _kMax;

        /// <summary>
        /// The current iteration.
        /// </summary>
        private int _k;

        /// <summary>
        /// The starting temperature.
        /// </summary>
        private readonly double _startingTemperature;

        /// <summary>
        /// The ending temperature.
        /// </summary>
        private readonly double _endingTemperature;

        /// <summary>
        /// The current temperature.
        /// </summary>
        private double _currentTemperature;

        /// <summary>
        /// The number of cycles to try at each temperature.
        /// </summary>
        private int _cycles = 100;

        /// <summary>
        /// The last probability of accepting a new non-improving move.
        /// </summary>
        private double _lastProbability;

        /// <summary>
        /// Construct the Simulated Annealing trainer. 
        /// </summary>
        /// <param name="theKMax">The maximum number of iterations.</param>
        /// <param name="theStartingTemperature">The starting temperature.</param>
        /// <param name="theEndingTemperature">The ending temperature.</param>
        protected DiscreteAnneal(int theKMax, double theStartingTemperature, double theEndingTemperature)
        {
            _kMax = theKMax;
            _startingTemperature = theStartingTemperature;
            _endingTemperature = theEndingTemperature;
        }

        /// <summary>
        /// The correct temperature for the current iteration.
        /// </summary>
        /// <returns>The correct temperature for the current iteration.</returns>
        public virtual double CoolingSchedule()
        {
            double ex = (double)_k / _kMax;
            return _startingTemperature * Math.Pow(_endingTemperature / _startingTemperature, ex);
        }

        /// <summary>
        /// Perform one training iteration.  This will execute the specified number of cycles at the current
        /// temperature.
        /// </summary>
        public void Iteration()
        {

            // Is this the first time through, if so, then setup.
            if (_k == 0)
            {
                _currentScore = Evaluate();
                FoundNewBest();
                _globalBestScore = _currentScore;
            }

            // incrament the current iteration counter
            _k++;

            // obtain the correct temperature
            _currentTemperature = CoolingSchedule();

            // perform the specified number of cycles
            for (int cycle = 0; cycle < _cycles; cycle++)
            {
                // backup current state
                BackupState();

                // randomize the method
                MoveToNeighbor();

                // did we improve it?  Only keep the new method if it improved (greedy).
                double trialScore = Evaluate();

                // was this iteration an improvement?  If so, always keep.
                bool keep = false;

                if (trialScore < _currentScore)
                {
                    // it was better, so always keep it
                    keep = true;
                }
                else
                {
                    // it was worse, so we might keep it
                    _lastProbability = CalcProbability(_currentScore, trialScore, _currentTemperature);
                    if (_lastProbability > _rnd.NextDouble())
                    {
                        keep = true;
                    }
                }

                // should we keep this position?
                if (keep)
                {
                    _currentScore = trialScore;
                    // better than global error
                    if (trialScore < _globalBestScore)
                    {
                        _globalBestScore = trialScore;
                        FoundNewBest();
                    }
                }
                else
                {
                    // do not keep this position
                    RestoreState();
                }
            }
        }

        /// <summary>
        /// Backup the current position (or state).
        /// </summary>
        public abstract void BackupState();

        /// <summary>
        /// Restore the current position (or state).
        /// </summary>
        public abstract void RestoreState();

        /// <summary>
        /// Handle the fact that we found a new global best.
        /// </summary>
        public abstract void FoundNewBest();

        /// <summary>
        /// Move to a neighbor position.
        /// </summary>
        public abstract void MoveToNeighbor();

        /// <summary>
        /// Evaluate the current position.
        /// </summary>
        /// <returns>The score.</returns>
        public abstract double Evaluate();

        /// <summary>
        /// True, if training has reached the last iteration.
        /// </summary>
        public bool Done
        {
            get
            {
                return _k >= _kMax;
            }
        }

        /// <summary>
        /// The best score found so far.
        /// </summary>
        public double BestScore
        {
            get
            {
                return _globalBestScore;
            }
        }

        /// <summary>
        /// Calculate the probability that a worse solution will be accepted.  The higher the temperature the 
        /// more likely this will happen. 
        /// </summary>
        /// <param name="ecurrent">The current energy (or score/error).</param>
        /// <param name="enew">The new energy (or score/error).</param>
        /// <param name="t">The current temperature.</param>
        /// <returns>The probability of accepting a worse solution.</returns>
        public virtual double CalcProbability(double ecurrent, double enew, double t)
        {
            return Math.Exp(-(Math.Abs(enew - ecurrent) / t));
        }

        /// <summary>
        /// The current iteration.
        /// </summary>
        public int K
        {
            get
            {
                return _k;
            }
        }

        /// <summary>
        /// The number of cycles per iteration.
        /// </summary>
        public int Cycles
        {
            get
            {
                return _cycles;
            }
            set
            {
                _cycles = value;
            }
        }

        /// <summary>
        /// Returns the current status of the algorithm.
        /// </summary>
        public string Status
        {
            get
            {
                var result = new StringBuilder();
                result.Append("k=");
                result.Append(_k);
                result.Append(",kMax=");
                result.Append(_kMax);
                result.Append(",t=");
                result.Append(_currentTemperature);
                result.Append(",prob=");
                result.Append(_lastProbability);
                return result.ToString();
            }
        }
    }
}
