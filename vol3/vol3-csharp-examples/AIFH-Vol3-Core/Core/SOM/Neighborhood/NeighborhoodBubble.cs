using System;

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    /// A neighborhood function that uses a simple bubble. A radius is defined, and
    /// any neuron that is plus or minus that width from the winning neuron will be
    /// updated as a result of training.
    /// </summary>
    public class NeighborhoodBubble : INeighborhoodFunction
    {
        /// <summary>
        /// The radius of the bubble.
        /// </summary>
        public double Radius { get; set; }
        
        /// <summary>
        /// Create a bubble neighborhood function that will return 1.0 (full update)
        /// for any neuron that is plus or minus the width distance from the winning
        /// neuron.
        /// </summary>
        /// <param name="radius">The width of the bubble, this is the distance that the neuron
        ///  can be from the winning neuron.The true width, across the
        ///  bubble, is actually two times this parameter.</param>
        public NeighborhoodBubble(int radius)
        {
            Radius = radius;
        }
        
        /// <summary>
        /// Determine how much the current neuron should be affected by training
        /// based on its proximity to the winning neuron.
        /// </summary>
        /// <param name="currentNeuron">THe current neuron being evaluated.</param>
        /// <param name="bestNeuron">The winning neuron.</param>
        /// <returns>The ratio for this neuron's adjustment.</returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            int distance = Math.Abs(bestNeuron - currentNeuron);
            if (distance <= Radius)
            {
                return 1.0;
            }
            else
            {
                return 0.0;
            }
        }
    }
}
