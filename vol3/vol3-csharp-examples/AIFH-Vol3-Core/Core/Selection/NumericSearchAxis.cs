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

using AIFH_Vol3.Core.Randomize;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    ///     A numeric range search axis.
    /// </summary>
    public class NumericSearchAxis : ISearchAxis
    {
        /// <summary>
        ///     The current position in the range.
        /// </summary>
        private double _currentState;

        /// <summary>
        ///     The start of the range.
        /// </summary>
        private readonly double _start;

        /// <summary>
        ///     The step in the range.
        /// </summary>
        private readonly double _step;

        /// <summary>
        ///     The end of the range.
        /// </summary>
        private readonly double _stop;

        /// <summary>
        ///     Construct a numeric range axis.
        /// </summary>
        /// <param name="start">The start of the range.</param>
        /// <param name="stop">Where to stop in the range.</param>
        /// <param name="step">The step for the range.</param>
        public NumericSearchAxis(double start, double stop, double step)
        {
            _start = start;
            _stop = stop;
            _step = step;
        }

        /// <summary>
        ///     The start of the range.
        /// </summary>
        public double Start
        {
            get { return _start; }
        }

        /// <summary>
        ///     The end of the range.
        /// </summary>
        public double Stop
        {
            get { return _stop; }
        }

        /// <summary>
        ///     The step value for the range.
        /// </summary>
        public double Step
        {
            get { return _step; }
        }

        /// <inheritdoc />
        public void Reset()
        {
            _currentState = _start;
        }

        /// <inheritdoc />
        public bool Advance()
        {
            _currentState += _step;
            if (_currentState >= _stop)
            {
                _currentState = _start;
                return true;
            }
            return false;
        }

        /// <inheritdoc />
        public object CurrentState()
        {
            return _currentState;
        }

        /// <inheritdoc />
        public object Sample(IGenerateRandom rnd)
        {
            return rnd.NextDouble(_start, _stop);
        }
    }
}