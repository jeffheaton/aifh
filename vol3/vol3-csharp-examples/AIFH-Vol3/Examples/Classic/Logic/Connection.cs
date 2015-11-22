using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    /// Connection between two neurons.
    /// </summary>
    public class Connection
    {
        /// <summary>
        /// The weight of the connection.
        /// </summary>
        public double Weight { get; set; }

        /// <summary>
        /// The parent/source.
        /// </summary>
        public INeuron Parent { get; set; }
        
        /// <summary>
        /// Create a connection. 
        /// </summary>
        /// <param name="weight">The weight.</param>
        /// <param name="parent">The parent/source neuron.</param>
        public Connection(double weight, INeuron parent)
        {
            Weight = weight;
            Parent = parent;
        }
        
    }
}
