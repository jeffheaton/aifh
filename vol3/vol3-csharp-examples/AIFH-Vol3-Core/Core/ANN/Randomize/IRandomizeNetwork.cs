using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Randomize
{
    /// <summary>
    /// Randomize a neural network.
    /// </summary>
    public interface IRandomizeNetwork
    {
        /// <summary>
        /// Randomize a neural network.
        /// </summary>
        /// <param name="network">The neural network to randomize.</param>
        void Randomize(BasicNetwork network);
    }
}
