using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Randomize
{
    /// <summary>
    /// Neural network randomizer that simply assigns each weight and bias to a uniform random number between a high
    /// and low range.
    /// </summary>
    public class RangeRandomizeNetwork: AbstractRandomizeNetwork
    {
        /// <summary>
        /// The low end of the range.
        /// </summary>
        private readonly double _low;

        /// <summary>
        /// The high end of the range.
        /// </summary>
        private readonly double _high;

        /// <summary>
        /// Construct the range randomizer.
        /// </summary>
        /// <param name="theLow">The low end of the range.</param>
        /// <param name="theHigh">The high end of the range.</param>
        public RangeRandomizeNetwork(double theLow, double theHigh)
        {
            _low = theLow;
            _high = theHigh;
        }

        /// <summary>
        /// Create a range randomizer between -1 and 1.
        /// </summary>
        public RangeRandomizeNetwork(): this(-1, 1)
        {
        }

        /// <inheritdoc/>
        public override void Randomize(BasicNetwork network)
        {
            for (int i = 0; i < network.Weights.Length; i++)
            {
                network.Weights[i] = Rnd.NextDouble(_low, _high);
            }
        }

    }
}
