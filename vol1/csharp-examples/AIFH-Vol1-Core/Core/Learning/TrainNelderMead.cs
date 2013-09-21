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

namespace AIFH_Vol1.Core.Learning
{
    /// <summary>
    ///     The Nelder-Mead method is a commonly used parameter optimization method that
    ///     can be used for machine learning. It typically provides a good error
    ///     rate and is relatively fast.
    ///     Nelder-Mead must build a simplex, which is an n*(n+1) matrix of weights. If
    ///     you have a large number of weights, this matrix can quickly overflow memory.
    ///     The biggest enhancement that is needed for this trainer is to make use of
    ///     multi-threaded code to evaluate the speed evaluations when training on a
    ///     multi-core.
    ///     This implementation is based on the source code provided by John Burkardt
    ///     (http://people.sc.fsu.edu/~jburkardt/)
    ///     http://people.sc.fsu.edu/~jburkardt/c_src/asa047/asa047.c
    /// </summary>
    public class TrainNelderMead:ILearningMethod
    {
        /// <summary>
        ///     Used to calculate the centroid.
        /// </summary>
        public const double Ccoeff = 0.5;

        public const double Ecoeff = 2.0;
        public const double Eps = 0.001;
        public const double Rcoeff = 1.0;
        private readonly IMachineLearningAlgorithm _algorithm;
        private readonly int _konvge;

        private readonly int _nn;
        private readonly double[] _p;
        private readonly double[] _p2Star;
        private readonly double[] _pbar;
        private readonly double[] _pstar;
        private readonly double _rq;
        private readonly IScoreFunction _score;
        private readonly double[] _start;
        private readonly double[] _step;
        private readonly double[] _trainedWeights;
        private readonly double[] _y;

        /// <summary>
        ///     True if the network has converged, and no further training is needed.
        /// </summary>
        private bool _converged;

        private double _del;

        private int _jcount;

        private double _lastError;

        public TrainNelderMead(IMachineLearningAlgorithm theAlgorithm, IScoreFunction theScore)
            : this(theAlgorithm, theScore, 100)
        {
        }

        public TrainNelderMead(IMachineLearningAlgorithm theAlgorithm, IScoreFunction theScore, double stepValue)
        {
            _algorithm = theAlgorithm;
            _score = theScore;

            _start = (double[]) _algorithm.LongTermMemory.Clone();
            _trainedWeights = (double[]) _algorithm.LongTermMemory.Clone();

            int n = _start.Length;

            _p = new double[n*(n + 1)];
            _pstar = new double[n];
            _p2Star = new double[n];
            _pbar = new double[n];
            _y = new double[n + 1];

            _nn = n + 1;
            _del = 1.0;
            _rq = 0.000001*n;

            _step = new double[_start.Length];
            _jcount = _konvge = 500;
            for (int i = 0; i < _step.Length; i++)
            {
                _step[i] = stepValue;
            }
        }

        /// <inheritdoc />
        public bool Done
        {
            get { return _converged; }
        }

        /// <inheritdoc />
        public String Status
        {
            get { return ""; }
        }

        /// <inheritdoc />
        public double LastError
        {
            get { return _lastError; }
        }

        /// <summary>
        ///     Calculate the error for the neural network with a given set of weights.
        /// </summary>
        /// <param name="weights">The weights to use.</param>
        /// <returns>The current error.</returns>
        public double Fn(double[] weights)
        {
            Array.Copy(weights, 0, _algorithm.LongTermMemory, 0, weights.Length);
            return _score.CalculateScore(_algorithm);
        }

        /// <inheritdoc />
        public void Iteration()
        {
            if (_converged)
            {
                return;
            }

            int n = _start.Length;

            Array.Copy(_start, 0, _p, n*n, n);
            _y[n] = Fn(_start);
            for (int j = 0; j < n; j++)
            {
                double x = _start[j];
                _start[j] = _start[j] + _step[j]*_del;
                Array.Copy(_start, 0, _p, j*n, n);
                _y[j] = Fn(_start);
                _start[j] = x;
            }
            /*
             * The simplex construction is complete.
             *
             * Find highest and lowest Y values. YNEWLO = Y(IHI) indicates the
             * vertex of the simplex to be replaced.
             */
            double ylo = _y[0];
            int ilo = 0;

            for (int i = 1; i < _nn; i++)
            {
                if (_y[i] < ylo)
                {
                    ylo = _y[i];
                    ilo = i;
                }
            }
            /*
             * Inner loop.
             */
            double ynewlo;

            double z;
            for (;;)
            {
                /*
                 * if (kcount <= icount) { break; }
                 */
                ynewlo = _y[0];
                int ihi = 0;

                for (int i = 1; i < _nn; i++)
                {
                    if (ynewlo < _y[i])
                    {
                        ynewlo = _y[i];
                        ihi = i;
                    }
                }
                /*
                 * Calculate PBAR, the centroid of the simplex vertices excepting
                 * the vertex with Y value YNEWLO.
                 */
                for (int i = 0; i < n; i++)
                {
                    z = 0.0;
                    for (int j = 0; j < _nn; j++)
                    {
                        z = z + _p[i + j*n];
                    }
                    z = z - _p[i + ihi*n];
                    _pbar[i] = z/n;
                }
                /*
                 * Reflection through the centroid.
                 */
                for (int i = 0; i < n; i++)
                {
                    _pstar[i] = _pbar[i] + Rcoeff
                               *(_pbar[i] - _p[i + ihi*n]);
                }
                double ystar = Fn(_pstar);
                /*
                 * Successful reflection, so extension.
                 */
                double y2Star;
                if (ystar < ylo)
                {
                    for (int i = 0; i < n; i++)
                    {
                        _p2Star[i] = _pbar[i] + Ecoeff
                                    *(_pstar[i] - _pbar[i]);
                    }
                    y2Star = Fn(_p2Star);
                    /*
                     * Check extension.
                     */
                    if (ystar < y2Star)
                    {
                        Array.Copy(_pstar, 0, _p, ihi*n, n);
                        _y[ihi] = ystar;
                    }
                        /*
                     * Retain extension or contraction.
                     */
                    else
                    {
                        Array.Copy(_p2Star, 0, _p, ihi*n, n);
                        _y[ihi] = y2Star;
                    }
                }
                    /*
                 * No extension.
                 */
                else
                {
                    int l = 0;
                    for (int i = 0; i < _nn; i++)
                    {
                        if (ystar < _y[i])
                        {
                            l = l + 1;
                        }
                    }

                    if (1 < l)
                    {
                        Array.Copy(_pstar, 0, _p, ihi*n, n);
                        _y[ihi] = ystar;
                    }
                        /*
                     * Contraction on the Y(IHI) side of the centroid.
                     */
                    else if (l == 0)
                    {
                        for (int i = 0; i < n; i++)
                        {
                            _p2Star[i] = _pbar[i] + Ccoeff
                                        *(_p[i + ihi*n] - _pbar[i]);
                        }
                        y2Star = Fn(_p2Star);
                        /*
                         * Contract the whole simplex.
                         */
                        if (_y[ihi] < y2Star)
                        {
                            for (int j = 0; j < _nn; j++)
                            {
                                for (int i = 0; i < n; i++)
                                {
                                    _p[i + j*n] = (_p[i + j*n] + _p[i
                                                                 + ilo*n])*0.5;
                                    _trainedWeights[i] = _p[i + j*n];
                                }
                                _y[j] = Fn(_trainedWeights);
                            }
                            ylo = _y[0];
                            ilo = 0;

                            for (int i = 1; i < _nn; i++)
                            {
                                if (_y[i] < ylo)
                                {
                                    ylo = _y[i];
                                    ilo = i;
                                }
                            }
                            continue;
                        }
                            /*
                         * Retain contraction.
                         */
                        Array.Copy(_p2Star, 0, _p, ihi*n, n);
                        _y[ihi] = y2Star;
                    }
                        /*
                     * Contraction on the reflection side of the centroid.
                     */
                    else if (l == 1)
                    {
                        for (int i = 0; i < n; i++)
                        {
                            _p2Star[i] = _pbar[i] + Ccoeff
                                        *(_pstar[i] - _pbar[i]);
                        }
                        y2Star = Fn(_p2Star);
                        /*
                         * Retain reflection?
                         */
                        if (y2Star <= ystar)
                        {
                            Array.Copy(_p2Star, 0, _p, ihi*n, n);
                            _y[ihi] = y2Star;
                        }
                        else
                        {
                            Array.Copy(_pstar, 0, _p, ihi*n, n);
                            _y[ihi] = ystar;
                        }
                    }
                }
                /*
                 * Check if YLO improved.
                 */
                if (_y[ihi] < ylo)
                {
                    ylo = _y[ihi];
                    ilo = ihi;
                }
                _jcount = _jcount - 1;

                if (0 < _jcount)
                {
                    continue;
                }
                /*
                 * Check to see if minimum reached.
                 */
                // if (icount <= kcount)
                {
                    _jcount = _konvge;

                    z = 0.0;
                    for (int i = 0; i < _nn; i++)
                    {
                        z = z + _y[i];
                    }
                    double x = z/_nn;

                    z = 0.0;
                    for (int i = 0; i < _nn; i++)
                    {
                        z = z + Math.Pow(_y[i] - x, 2);
                    }

                    if (z <= _rq)
                    {
                        break;
                    }
                }
            }
            /*
             * Factorial tests to check that YNEWLO is a local minimum.
             */
            Array.Copy(_p, ilo*n, _trainedWeights, 0, n);
            ynewlo = _y[ilo];

            bool fault = false;

            for (int i = 0; i < n; i++)
            {
                _del = _step[i]*Eps;
                _trainedWeights[i] += _del;
                z = Fn(_trainedWeights);
                if (z < ynewlo)
                {
                    fault = true;
                    break;
                }
                _trainedWeights[i] = _trainedWeights[i] - _del
                                    - _del;
                z = Fn(_trainedWeights);
                if (z < ynewlo)
                {
                    fault = true;
                    break;
                }
                _trainedWeights[i] += _del;
            }

            if (!fault)
            {
                _converged = true;
            }
            else
            {
                /*
                 * Restart the procedure.
                 */
                Array.Copy(_trainedWeights, 0, _start, 0, n);
                _del = Eps;
            }

            _lastError = ynewlo;
            Array.Copy(_trainedWeights, 0, _algorithm.LongTermMemory, 0, _trainedWeights.Length);
        }

        /// <inheritdoc />
        public void FinishTraining()
        {
        }
    }
}