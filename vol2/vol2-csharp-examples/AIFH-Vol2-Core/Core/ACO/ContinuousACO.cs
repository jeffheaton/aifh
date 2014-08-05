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
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.ACO
{
    /// <summary>
    /// This class implements continuous ant colony optimization (CACO)
    ///
    /// References:
    /// 
    /// Training Neural Networks with Ant Colony Optimization,
    /// Arun Pandian, Spring, 2013
    /// 
    /// Krzysztof Socha and Christian Blum. “An ant colony optimization algorithm for
    /// continuous optimization: application to feed-forward neural network training”, in
    /// Springer London (2007).
    ///
    /// M.Dorigo, V.Maniezzo, and A.Colorni. “Ant System: Optimization by a colony of
    /// cooperating agents”, in IEEE Transactions on Systems, Man, and Cybernetics,
    /// 1996.
    /// </summary>
    public class ContinuousACO : ILearningMethod
    {
        /// <summary>
        /// Sigma constant. Minimum standard deviation.
        /// </summary>
        public const double CONST_SIGMA = 0.1;

        /// <summary>
        /// Q constant.  Weighting exponent factor.
        /// </summary>
        public const double CONST_Q = 0.08;

        /// <summary>
        /// The population of ants.
        /// </summary>
        private readonly ContinuousAnt[] _population;

        /// <summary>
        /// The population size.
        /// </summary>
        private readonly int _populationSize;

        /// <summary>
        /// The parameter count.
        /// </summary>
        private int _paramCount = 0;

        /// <summary>
        /// The weighting of each ant.
        /// </summary>
        private double[] _weighting;

        /// <summary>
        /// The sum of the weighting.
        /// </summary>
        private double _sumWeighting = 0;

        /// <summary>
        /// Epsilon, learning rate.
        /// </summary>
        public double Epsilon { get; set; }

        /// <summary>
        /// Random number generation.
        /// </summary>
        public IGenerateRandom Random { get; set; }

        /// <summary>
        /// The algorithm that we are fitting.
        /// </summary>
        private IMLMethod _algorithm;

        /// <summary>
        /// The score function.
        /// </summary>
        private IScoreFunction _score;

        /// <summary>
        /// The constructor. 
        /// </summary>
        /// <param name="theAlgorithm">The algorithm to fit.</param>
        /// <param name="theScore">The score function.</param>
        /// <param name="thePopulationSize">The population size.</param>
        public ContinuousACO(IMLMethod theAlgorithm, IScoreFunction theScore, int thePopulationSize)
        {
            Epsilon = .75;

            _algorithm = theAlgorithm;
            _populationSize = thePopulationSize;
            _score = theScore;
            Random = new MersenneTwisterGenerateRandom();
            _paramCount = theAlgorithm.LongTermMemory.Length;

            _population = new ContinuousAnt[thePopulationSize * 2];
            _weighting = new double[thePopulationSize];
            for (int i = 0; i < _population.Length; i++)
            {
                _population[i] = new ContinuousAnt(_paramCount, _score.ShouldMinimize);
                for (int j = 0; j < _paramCount; j++)
                {
                    _population[i].Params[j] = Random.NextDouble(-1, 1);
                }
            }

            UpdateScore();
            Array.Sort(_population);
            ComputeWeighting();
            SampleSolutions();
            Array.Sort(_population);

        }

        /// <summary>
        /// Update the score.
        /// </summary>
        private void UpdateScore()
        {

            foreach (ContinuousAnt aPopulation in _population)
            {
                Array.Copy(aPopulation.Params, _algorithm.LongTermMemory, _paramCount);
                aPopulation.Score = _score.CalculateScore(_algorithm);
            }
        }

        /// <summary>
        /// Compute the weighting for each ant.
        /// </summary>
        private void ComputeWeighting()
        {
            _sumWeighting = 0;
            for (int i = 0; i < _populationSize; i++)
            {
                double exponent = (i * i) / (2 * CONST_Q * CONST_Q * _populationSize * _populationSize);
                _weighting[i] =
                        (1 / (0.1 * Math.Sqrt(2 * Math.PI))) * Math.Pow(Math.E, -exponent);
                _sumWeighting += _weighting[i];
            }
        }

        /// <summary>
        /// Compute the standard deviation. 
        /// </summary>
        /// <param name="x">The parameter to compute for.</param>
        /// <param name="l">The population member.</param>
        /// <returns>The standard deviation.</returns>
        private double ComputeSD(int x, int l)
        {
            double sum = 0.0;
            for (int i = 0; i < _populationSize; i++)
            {
                sum += Math.Abs(_population[i].Params[x] - _population[l].Params[x]) / (_populationSize - 1);
            }
            if (sum < AIFH.DefaultPrecision)
            {
                return CONST_SIGMA;
            }
            return (Epsilon * sum);
        }

        /// <summary>
        /// Select a probability distribution function (PDF). 
        /// </summary>
        /// <returns>The PDF index.</returns>
        private int SelectPDF()
        {
            int l = 0;
            double temp = 0;

            double r = Random.NextDouble();
            for (int i = 0; i < _populationSize; i++)
            {
                temp += _weighting[i] / _sumWeighting;
                if (r < temp)
                {
                    l = i;
                    break;
                }
            }
            return l;
        }

        /// <summary>
        /// Sample new parameters.
        /// </summary>
        private void SampleSolutions()
        {
            for (int i = _populationSize; i < _population.Length; i++)
            {
                int pdf = SelectPDF();
                for (int j = 0; j < _paramCount; j++)
                {
                    double sigma = ComputeSD(j, pdf);
                    double mu = _population[pdf].Params[j];
                    double d = (Random.NextGaussian() * sigma) + mu;
                    _population[i].Params[j] = d;
                }
            }
        }

        /// <inheritdoc/>
        public void Iteration()
        {
            ComputeWeighting();
            SampleSolutions();
            UpdateScore();
            Array.Sort(_population);
        }

        /// <inheritdoc/>
        public double LastError
        {
            get
            {
                return _population[0].Score;
            }
        }

        /// <inheritdoc/>
        public bool Done
        {
            get
            {
                return false;
            }
        }

        /// <inheritdoc/>
        public String Status
        {
            get
            {
                return "";
            }
        }

        /// <inheritdoc/>
        public void FinishTraining()
        {
            Array.Copy(_population[0].Params, _algorithm.LongTermMemory, _algorithm.LongTermMemory.Length);
        }
    }
}
