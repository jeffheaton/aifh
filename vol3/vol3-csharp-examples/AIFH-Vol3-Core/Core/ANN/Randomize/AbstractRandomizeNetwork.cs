using AIFH_Vol3.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Randomize
{
    /// <summary>
    /// Base class for network randomizers that are based on a GenerateRandom PRNG.
    /// </summary>
    public abstract class AbstractRandomizeNetwork : IRandomizeNetwork
    {
        /// <summary>
        /// The random number generator.
        /// </summary>
        private IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        /// The random number generator.
        /// </summary>
        public IGenerateRandom Rnd
        {
            get
            {
                return _rnd;
            }
            set
            {
                _rnd = value;
            }
        }

        /// <inheritdoc/>
        public abstract void Randomize(BasicNetwork network);
    }
}
