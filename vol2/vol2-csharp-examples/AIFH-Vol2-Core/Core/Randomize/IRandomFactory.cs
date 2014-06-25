using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Randomize
{
    /// <summary>
    /// A factory to create random number generators of a specific type.
    /// </summary>
    public interface IRandomFactory
    {
        /// <summary>
        /// Factor a random number generator.
        /// </summary>
        /// <returns>The random number generator.</returns>
        IGenerateRandom Factor();

        /// <summary>
        /// Factor a factory, just like this one.
        /// </summary>
        /// <returns></returns>
        IRandomFactory FactorFactory();
    }
}
