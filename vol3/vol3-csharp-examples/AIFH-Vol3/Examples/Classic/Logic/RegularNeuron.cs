using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    /// A regular neuron, can function as either a hidden our output.
    /// </summary>
    public class RegularNeuron: INeuron
    {
        /// <summary>
        /// The parents to this neuron.
        /// </summary>
        private readonly IList<Connection> _parents = new List<Connection>();

        /// <summary>
        /// The bias.
        /// </summary>
        private readonly double _bias;
        
        /// <summary>
        /// Construct the neuron. 
        /// </summary>
        /// <param name="bias">The neuron's bias.</param>
        public RegularNeuron(double bias)
        {
            _bias = bias;
        }

        /// <inheritdoc />
        public double Compute()
        {
            double sum = _bias;
            foreach (Connection c in _parents)
            {
                sum += c.Weight * c.Parent.Compute();
            }

            if (sum >= 0.5)
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }

        /// <summary>
        /// The parent neurons.
        /// </summary>
        public IList<Connection> Parents
        {
            get { return _parents; }
        }
    }
}
