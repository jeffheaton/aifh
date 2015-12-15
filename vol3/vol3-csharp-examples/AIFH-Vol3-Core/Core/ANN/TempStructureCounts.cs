using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    /// Simple holder class that keeps the count of the neurons and weights as a neural network is built.  This class is
    /// temporary and is only used briefly while the neural network structure is finalized.
    /// </summary>
    public class TempStructureCounts
    {
        /// <summary>
        /// The number of neurons needed so far.
        /// </summary>
        public int NeuronCount { get; set; }

        /// <summary>
        /// The number of weights needed so far.
        /// </summary>
        public int WeightCount { get; set; }

        /// <summary>
        /// Add to the neuron count.
        /// </summary>
        /// <param name="i">The amount to add.</param>
        public void AddNeuronCount(int i)
        {
            NeuronCount += i;
        }

        /// <summary>
        /// Add to the weight count.
        /// </summary>
        /// <param name="i">The amount to add.</param>
        public void AddWeightCount(int i)
        {
            WeightCount += i;
        }
    }
}
