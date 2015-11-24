
namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    /// A very simple neighborhood function that will return 1.0 (full effect) for
    /// the winning neuron, and 0.0 (no change) for everything else.
    /// </summary>
    public class NeighborhoodSingle : INeighborhoodFunction
    {
        /// <summary>
        /// Determine how much the current neuron should be affected by training
        /// based on its proximity to the winning neuron.
        /// </summary>
        /// <param name="currentNeuron">THe current neuron being evaluated.</param>
        /// <param name="bestNeuron">The winning neuron.</param>
        /// <returns>The ratio for this neuron's adjustment.</returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            if (currentNeuron == bestNeuron)
            {
                return 1.0;
            }
            else
            {
                return 0.0;
            }
        }

        /// <summary>
        /// The radius for this neighborhood function is always 1.
        /// </summary>
        public double Radius
        {
            get { return 1; }
            set
            {
                // no effect on this type }
            }
        }
    }
}
