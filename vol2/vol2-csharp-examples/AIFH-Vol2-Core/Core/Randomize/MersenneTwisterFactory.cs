using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Randomize
{
    /// <summary>
    /// A factory to create Mersenne Twister PRNG objects.
    /// </summary>
    [Serializable]
    public class MersenneTwisterFactory : IRandomFactory
    {
        /// <inheritdoc/>
        public IGenerateRandom Factor()
        {
            return new MersenneTwisterGenerateRandom();
        }

        /// <inheritdoc/>
        public IRandomFactory FactorFactory()
        {
            return new MersenneTwisterFactory();
        }
    }
}
