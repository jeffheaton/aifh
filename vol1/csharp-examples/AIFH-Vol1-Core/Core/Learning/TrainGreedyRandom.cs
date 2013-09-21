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
using AIFH_Vol1.Core.Learning.Score;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Core.Learning
{
    /// <summary>
    ///     The Greedy Random learning algorithm is a very primitive random-walk algorithm that only takes steps that serve
    ///     to move the Machine Learning algorithm to a more optimal position.  This learning algorithm essentially chooses
    ///     random locations for the long term memory until a better set is found.
    ///     http://en.wikipedia.org/wiki/Random_walk
    /// </summary>
    public class TrainGreedyRandom : ILearningMethod
    {
        /// <summary>
        ///     True, if we are minimizing the score function.
        /// </summary>
        private readonly bool _shouldMinimize;

        /// <summary>
        ///     The Machine Learning algorithm to optimize.
        /// </summary>
        private readonly IMachineLearningAlgorithm _algorithm;

        /// <summary>
        ///     The score function.
        /// </summary>
        private readonly IScoreFunction _score;

        /// <summary>
        ///     The high range for random number selection.
        /// </summary>
        private double _highRange = 10;

        /// <summary>
        ///     The last error.
        /// </summary>
        private double _lastError;

        /// <summary>
        ///     The low range for random number selection.
        /// </summary>
        private double _lowRange = -10;

        /// <summary>
        ///     The random number generator to use.
        /// </summary>
        private IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        ///     Construct a greedy random algorithm.
        /// </summary>
        /// <param name="theShouldMinimize">True, if we should minimize.</param>
        /// <param name="theAlgorithm">The algorithm to optimize.</param>
        /// <param name="theScore">The score function.</param>
        public TrainGreedyRandom(bool theShouldMinimize, IMachineLearningAlgorithm theAlgorithm, IScoreFunction theScore)
        {
            _algorithm = theAlgorithm;
            _score = theScore;
            _shouldMinimize = theShouldMinimize;

            // Set the last error to a really bad value so it will be reset on the first iteration.
            _lastError = _shouldMinimize ? double.PositiveInfinity : Double.NegativeInfinity;
        }

        /// <summary>
        ///     The low range.
        /// </summary>
        public double LowRange
        {
            get { return _lowRange; }
            set { _lowRange = value; }
        }

        /// <summary>
        ///     The high range.
        /// </summary>
        public double HighRange
        {
            get { return _highRange; }
            set { _highRange = value; }
        }

        public IGenerateRandom Rnd
        {
            get { return _rnd; }
            set { _rnd = value; }
        }

        /// <summary>
        ///     Perform iteration.
        /// </summary>
        public void Iteration()
        {
            int len = _algorithm.LongTermMemory.Length;

            // backup current state
            var oldState = new double[len];
            Array.Copy(_algorithm.LongTermMemory, 0, oldState, 0, len);

            // randomize the method
            PerformRandomize(_algorithm.LongTermMemory);

            // did we improve it?  Only keep the new method if it improved (greedy).
            double currentError = _score.CalculateScore(_algorithm);

            if ((currentError < _lastError) ? _shouldMinimize : !_shouldMinimize)
            {
                _lastError = currentError;
            }
            else
            {
                Array.Copy(oldState, 0, _algorithm.LongTermMemory, 0, len);
            }
        }

        /// <summary>
        ///     Randomly move to a new location.  To specify a new randomization function, override this method.
        /// </summary>
        /// <param name="memory">The long term memory.</param>
        public void PerformRandomize(double[] memory)
        {
            for (int i = 0; i < memory.Length; i++)
            {
                memory[i] = _rnd.NextDouble(_lowRange, _highRange);
            }
        }

        /// <inheritdoc />
        public string Status
        {
            get
            {
                return "";
            }
        }

        /// <inheritdoc />
        public double LastError
        {
            get
            {
                return _lastError;
            }
        }

        /// <inheritdoc />
        public bool Done
        {
            get
            {
                return false;
            }
        }

        /// <inheritdoc />
        public void FinishTraining()
        {
        }
    }
}