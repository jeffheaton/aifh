using AIFH_Vol3.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    /// A numeric range search axis.
    /// </summary>
    public class NumericSearchAxis : ISearchAxis
    {
        /// <summary>
        /// The start of the range.
        /// </summary>
        private double _start;

        /// <summary>
        /// The end of the range.
        /// </summary>
        private double _stop;

        /// <summary>
        /// The step in the range.
        /// </summary>
        private double _step;

        /// <summary>
        /// The current position in the range.
        /// </summary>
        private double _currentState;

        /// <summary>
        /// Construct a numeric range axis. 
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
        /// The start of the range.
        /// </summary>
        public double Start
        {
            get
            {
                return _start;
            }
        }

        /// <summary>
        /// The end of the range.
        /// </summary>
        public double Stop
        {
            get
            {
                return _stop;
            }
        }

        /// <summary>
        /// The step value for the range.
        /// </summary>
        public double Step
        {
            get
            {
                return _step;
            }
        }

        /// <inheritdoc/>
        public void Reset()
        {
            _currentState = _start;
        }

        /// <inheritdoc/>
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

        /// <inheritdoc/>
        public Object CurrentState()
        {
            return _currentState;
        }

        /// <inheritdoc/>
        public Object Sample(IGenerateRandom rnd)
        {
            return rnd.NextDouble(_start, _stop);
        }
    }
}
