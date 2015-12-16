using AIFH_Vol3.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    /// A search axis.
    /// </summary>
    public interface ISearchAxis
    {
        /// <summary>
        /// Reset the axis back to the beginning.
        /// </summary>
        void Reset();
        
        /// <summary>
        /// Advance a position in the search. 
        /// </summary>
        /// <returns>True if there is more to read.</returns>
        bool Advance();
        
        /// <summary>
        /// The current state of the axis. 
        /// </summary>
        /// <returns>The current value of the axis.</returns>
        Object CurrentState();
        
        /// <summary>
        /// Sample a random value from the axis. 
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <returns>The value randomly picked.</returns>
        Object Sample(IGenerateRandom rnd);
    }
}
