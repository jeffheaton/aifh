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

using AIFH_Vol1.Core.Learning.Score;

namespace AIFH_Vol1.Core.Learning
{
    /// <summary>
    ///     Train using hill climbing.  Hill climbing can be used to optimize the long term memory of a Machine Learning
    ///     Algorithm. This is done by moving the current long term memory values to a new location if that new location
    ///     gives a better score from the scoring function.
    ///     http://en.wikipedia.org/wiki/Hill_climbing
    /// </summary>
    public class TrainHillClimb : ILearningMethod
    {
        /// <summary>
        ///     The machine learning algorithm to optimize.
        /// </summary>
        private readonly IMachineLearningAlgorithm _algorithm;

        /// <summary>
        ///     The candidate moves.
        /// </summary>
        private readonly double[] _candidate = new double[5];

        /// <summary>
        ///     The score function.
        /// </summary>
        private readonly IScoreFunction _score;

        /// <summary>
        ///     True, if we want to minimize the score function.
        /// </summary>
        private readonly bool _shouldMinimize;

        /// <summary>
        ///     The current step size.
        /// </summary>
        private readonly double[] _stepSize;

        /// <summary>
        ///     The last result from the score function.
        /// </summary>
        private double _lastError;

        /// <summary>
        ///     Construct a hill climbing algorithm.
        /// </summary>
        /// <param name="theShouldMinimize">True, if we should minimize.</param>
        /// <param name="theAlgorithm">The algorithm to optimize.</param>
        /// <param name="theScore">The scoring function.</param>
        /// <param name="acceleration">The acceleration for step sizes.</param>
        /// <param name="stepSize">The initial step sizes.</param>
        public TrainHillClimb(bool theShouldMinimize, IMachineLearningAlgorithm theAlgorithm, IScoreFunction theScore,
            double acceleration, double stepSize)
        {
            _algorithm = theAlgorithm;
            _score = theScore;
            _shouldMinimize = theShouldMinimize;

            _stepSize = new double[theAlgorithm.LongTermMemory.Length];
            for (int i = 0; i < theAlgorithm.LongTermMemory.Length; i++)
            {
                _stepSize[i] = stepSize;
            }

            _candidate[0] = -acceleration;
            _candidate[1] = -1/acceleration;
            _candidate[2] = 0;
            _candidate[3] = 1/acceleration;
            _candidate[4] = acceleration;

            // Set the last error to a really bad value so it will be reset on the first iteration.
            _lastError = _shouldMinimize ? double.PositiveInfinity : double.NegativeInfinity;
        }

        /// <summary>
        ///     Construct a hill climbing algorithm. Use acceleration of 1.2 and initial step size of 1.
        /// </summary>
        /// <param name="theShouldMinimize">True, if we should minimize.</param>
        /// <param name="theAlgorithm">The algorithm to optimize.</param>
        /// <param name="theScore">The scoring function.</param>
        public TrainHillClimb(bool theShouldMinimize, IMachineLearningAlgorithm theAlgorithm, IScoreFunction theScore)
            : this(theShouldMinimize, theAlgorithm, theScore, 1.2, 1)
        {
        }

        /// <inheritdoc />
        public void Iteration()
        {
            int len = _algorithm.LongTermMemory.Length;

            for (int i = 0; i < len; i++)
            {
                int best = -1;
                double bestScore = _shouldMinimize ? double.PositiveInfinity : double.NegativeInfinity;

                for (int j = 0; j < _candidate.Length; j++)
                {
                    _algorithm.LongTermMemory[i] += _stepSize[i]*_candidate[j];
                    double temp = _score.CalculateScore(_algorithm);
                    _algorithm.LongTermMemory[i] -= _stepSize[i]*_candidate[j];

                    if ((temp < bestScore) ? _shouldMinimize : !_shouldMinimize)
                    {
                        bestScore = temp;
                        _lastError = bestScore;
                        best = j;
                    }
                }

                if (best != -1)
                {
                    _algorithm.LongTermMemory[i] += _stepSize[i]*_candidate[best];
                    _stepSize[i] = _stepSize[i]*_candidate[best];
                }
            }
        }

        /// <inheritdoc />
        public bool Done
        {
            get { return false; }
        }

        /// <inheritdoc />
        public double LastError
        {
            get { return _lastError; }
        }

        /// <inheritdoc />
        public string Status
        {
            get { return ""; }
        }

        /// <inheritdoc />
        public void FinishTraining()
        {
        }
    }
}