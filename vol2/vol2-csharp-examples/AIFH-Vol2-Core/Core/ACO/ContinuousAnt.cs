using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.ACO
{
    /// <summary>
    /// An individual ant for continuous ACO.
    /// </summary>
    public class ContinuousAnt
    {
        /// <summary>
        /// The score for this ant.
        /// </summary>
        public double Score { get; set; }

        /// <summary>
        /// The parameters for this ant.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        /// True, if this ant should minimize.  This value should be the same for all ants.
        /// </summary>
        private bool _shouldMinimize;

        /// <summary>
        /// The constructor. 
        /// </summary>
        /// <param name="n">The number of parameters (dimensions).</param>
        /// <param name="theShouldMinimize">True, if we are minimizing.</param>
        public ContinuousAnt(int n, bool theShouldMinimize)
        {
            _params = new double[n];
            _shouldMinimize = theShouldMinimize;
        }

        /// <summary>
        /// The parameters for this ant.
        /// </summary>
        public double[] Params
        {
            get
            {
                return _params;
            }
        }

        /// <inheritdoc/>
        public int CompareTo(ContinuousAnt o)
        {
            if (_shouldMinimize)
            {
                return Score.CompareTo(o.Score);
            }
            else
            {
                return o.Score.CompareTo(Score);
            }
        }
    }
}
