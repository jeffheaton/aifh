// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
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
using AIFH_Vol3.Core.Learning.Score;
using AIFH_Vol3.Core.Randomize;

namespace AIFH_Vol3.Core.Learning
{
    /// <summary>
    ///     Train a Machine Learning Algorithm using Simulated Annealing.  Simulated Annealing is a Monte Carlo algorithm that
    ///     is based on annealing in metallurgy, a technique involving heating and controlled cooling of a material to increase
    ///     the size of its crystals and reduce their defects, both are attributes of the material that depend on its
    ///     thermodynamic free energy.
    ///     The Simulated Annealing algorithm works by randomly changing a vector of doubles.  This is the long term memory of
    ///     the Machine Learning algorithm.  While this happens a temperature is slowly decreased.  When this temperature is
    ///     higher, the Simulated Annealing algorithm is more likely to accept changes that have a higher error (or energy)
    ///     than the current state.
    ///     There are several important components to any Simulated Learning Algorithm:
    ///     First, the randomization technique.  This is performed by the method performRandomize.  To randomize differently,
    ///     override this method.
    ///     Secondly, the cooling schedule.  This determines how quickly the current temperature will fall.  This is controlled
    ///     by the coolingSchedule.  To define a different cooling schedule, override this method.
    ///     Finally, the probability of accepting a higher-error (energy) solution.  This is defined by a Probability
    ///     Distribution Function (PDF) contained in calcProbability.  To define a different PDF, override this method.
    ///     http://en.wikipedia.org/wiki/Simulated_annealing
    /// </summary>
    public class TrainAnneal : ILearningMethod
    {
        /// <summary>
        ///     The machine learning algorithm to optimize.
        /// </summary>
        private readonly IMLMethod _algorithm;

        /// <summary>
        ///     The ending temperature.  Do not set to zero, as many cooling schedules asymptotically approach zero.
        ///     Rather, use something close to zero, like 0.0001.
        /// </summary>
        private readonly double _endingTemperature;

        /// <summary>
        ///     The current best solution ever found.
        /// </summary>
        private readonly double[] _globalBest;

        /// <summary>
        ///     The maximum number of iterations.
        /// </summary>
        private readonly int _kMax;

        /// <summary>
        ///     The random number generator to use.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        ///     The scoring function, this determines the energy (error) of the current solution.
        /// </summary>
        private readonly IScoreFunction _score;

        /// <summary>
        ///     The starting temperature.
        /// </summary>
        private readonly double _startingTemperature;

        /// <summary>
        ///     The current error.
        /// </summary>
        private double _currentError;

        /// <summary>
        ///     The current temperature.
        /// </summary>
        private double _currentTemperature;

        /// <summary>
        ///     The number of random moves to try for each iteration.
        /// </summary>
        private int _cycles = 100;

        /// <summary>
        ///     The current error of best solution ever found.
        /// </summary>
        private double _globalBestError = double.PositiveInfinity;

        /// <summary>
        ///     The current iteration number.
        /// </summary>
        private int _k;

        /// <summary>
        ///     The probability for the last iteration cycle.
        /// </summary>
        private double _lastProbability;

        /// <summary>
        ///     Construct the simulated annealing trainer.  Use 1000 iterations and temperature from 400 to 0.0001.
        /// </summary>
        /// <param name="theAlgorithm">The algorithm to optimize.</param>
        /// <param name="theScore">The score function.</param>
        public TrainAnneal(IMLMethod theAlgorithm, IScoreFunction theScore)
            : this(theAlgorithm, theScore, 1000, 400, 0.0001)
        {
        }

        /// <summary>
        ///     Construct the simulated annealing trainer.
        /// </summary>
        /// <param name="theAlgorithm">The algorithm to optimize.</param>
        /// <param name="theScore">The score function.</param>
        /// <param name="theKMax">The max number of iterations.</param>
        /// <param name="theStartingTemperature">The starting temperature.</param>
        /// <param name="theEndingTemperature">The ending temperature.</param>
        public TrainAnneal(IMLMethod theAlgorithm, IScoreFunction theScore, int theKMax,
            double theStartingTemperature, double theEndingTemperature)
        {
            _algorithm = theAlgorithm;
            _score = theScore;
            _kMax = theKMax;
            _currentError = _score.CalculateScore(_algorithm);
            _startingTemperature = theStartingTemperature;
            _endingTemperature = theEndingTemperature;
            _globalBest = new double[theAlgorithm.LongTermMemory.Length];
            Array.Copy(_algorithm.LongTermMemory, 0, _globalBest, 0, _globalBest.Length);
        }


        /// <summary>
        ///     The current temperature.
        /// </summary>
        public double CurrentTemperature
        {
            get { return _currentTemperature; }
        }

        /// <summary>
        ///     The current iteration number.
        /// </summary>
        public int K
        {
            get { return _k; }
        }

        /// <summary>
        ///     The starting temperature.
        /// </summary>
        public double StartingTemperature
        {
            get { return _startingTemperature; }
        }

        /// <summary>
        ///     The ending temperature.
        /// </summary>
        public double EndingTemperature
        {
            get { return _endingTemperature; }
        }

        /// <summary>
        ///     The number of cycles per iteration.
        /// </summary>
        public int Cycles
        {
            get { return _cycles; }
            set { _cycles = value; }
        }

        /// <summary>
        ///     The last probability.
        /// </summary>
        public double LastProbability
        {
            get { return _lastProbability; }
        }

        /// <summary>
        ///     True, if we have reached the max iterations.
        /// </summary>
        public bool Done
        {
            get { return _k >= _kMax; }
        }

        /// <summary>
        ///     The error (or energy) from the last iteration.
        /// </summary>
        public double LastError
        {
            get { return _globalBestError; }
        }

        /// <inheritdoc />
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

        /// <inheritdoc />
        public void Iteration()
        {
            var len = _algorithm.LongTermMemory.Length;
            _k++;

            _currentTemperature = CoolingSchedule();

            for (var cycle = 0; cycle < _cycles; cycle++)
            {
                // backup current state
                var oldState = new double[len];
                Array.Copy(_algorithm.LongTermMemory, 0, oldState, 0, len);

                // randomize the method
                PerformRandomize(_algorithm.LongTermMemory);

                // did we improve it?  Only keep the new method if it improved (greedy).
                var trialError = _score.CalculateScore(_algorithm);

                // was this iteration an improvement?  If so, always keep.
                var keep = false;

                if (trialError < _currentError)
                {
                    keep = true;
                }
                else
                {
                    _lastProbability = CalcProbability(_currentError, trialError, _currentTemperature);
                    if (_lastProbability > _rnd.NextDouble())
                    {
                        keep = true;
                    }
                }

                if (keep)
                {
                    _currentError = trialError;
                    // better than global error
                    if (trialError < _globalBestError)
                    {
                        _globalBestError = trialError;
                        Array.Copy(_algorithm.LongTermMemory, 0, oldState, 0, len);
                        Array.Copy(_algorithm.LongTermMemory, 0, _globalBest, 0, len);
                    }
                }
                else
                {
                    Array.Copy(oldState, 0, _algorithm.LongTermMemory, 0, len);
                }
            }
        }

        /// <summary>
        ///     Copy the global best solution to the machine learning algorithm.  It is very important to call this method.
        /// </summary>
        public void FinishTraining()
        {
            Array.Copy(_globalBest, 0, _algorithm.LongTermMemory, 0, _globalBest.Length);
        }

        /// <summary>
        ///     The cooling schedule.  This is a Probability Distribution Function (PDF) that specifies the probability,
        ///     at a given temperature, of accepting a higher-energy move.
        /// </summary>
        /// <returns>The probability.</returns>
        public double CoolingSchedule()
        {
            var ex = _k/(double) _kMax;
            return _startingTemperature*Math.Pow(_endingTemperature/_startingTemperature, ex);
        }

        /// <summary>
        ///     Randomly move to a new location.  To specify a new randomization function, override this method.
        /// </summary>
        /// <param name="memory">The long term memory.</param>
        public void PerformRandomize(double[] memory)
        {
            for (var i = 0; i < memory.Length; i++)
            {
                var d = _rnd.NextGaussian()*3;
                memory[i] += d;
            }
        }

        /// <summary>
        ///     Calculate the probability that we will accept a move that takes us to a higher energy (higher error)
        ///     position.
        /// </summary>
        /// <param name="ecurrent">The current energy.</param>
        /// <param name="enew">The new energy if we move.</param>
        /// <param name="t">The current temperature.</param>
        /// <returns>The probability.</returns>
        public double CalcProbability(double ecurrent, double enew, double t)
        {
            return Math.Exp(-(Math.Abs(enew - ecurrent)/t));
        }
    }
}