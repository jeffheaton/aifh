using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    /// Defines the interface to simple neurons.
    /// </summary>
    public interface INeuron
    {
        /// <summary>
        /// Compute the value of the neuron.
        /// </summary>
        /// <returns>The computed value;</returns>
        double Compute();
    }
}
