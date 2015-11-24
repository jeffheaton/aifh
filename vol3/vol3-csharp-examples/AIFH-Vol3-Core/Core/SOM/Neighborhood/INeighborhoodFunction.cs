using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    ///  * Defines how a neighborhood function should work in competitive training. This
    /// is most often used in the training process for a self-organizing map.This
    /// function determines to what degree the training should take place on a
    /// neuron, based on its proximity to the "winning" neuron.
    /// </summary>
    public interface INeighborhoodFunction
    {
        /// <summary>
        /// Determine how much the current neuron should be affected by training
        /// based on its proximity to the winning neuron.
        /// </summary>
        /// <param name="currentNeuron">THe current neuron being evaluated.</param>
        /// <param name="bestNeuron">The winning neuron.</param>
        /// <returns>The ratio for this neuron's adjustment.</returns>
        double Function(int currentNeuron, int bestNeuron);

        /// <summary>
        /// The radius.
        /// </summary>
        double Radius { get; set; }
        
    }
}
