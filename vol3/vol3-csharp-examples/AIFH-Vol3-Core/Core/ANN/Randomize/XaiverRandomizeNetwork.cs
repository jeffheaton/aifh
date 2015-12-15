using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Randomize
{
    /// <summary>
    /// The Xaiver initialization (aka Glorot) weight initialization.  A very good weight initialization method that 
    /// provides very efficient training and relatively consistent results.As described by the following paper.
    ///
    /// Glorot, X., & Bengio, Y. (2010). Understanding the difficulty of training deep feedforward neural networks.
    /// In International conference on artificial intelligence and statistics(pp. 249-256).
    /// </summary>
    public class XaiverRandomizeNetwork : AbstractRandomizeNetwork
    {
        /// <summary>
        /// The Xaiver initialization works layer by layer.
        /// </summary>
        /// <param name="network">The network.</param>
        /// <param name="fromLayer">The source layer.</param>
        private void RandomizeLayer(BasicNetwork network, int fromLayer)
        {
            int fromCount = network.GetLayerTotalNeuronCount(fromLayer);
            int toCount = network.Layers[fromLayer + 1].Count;

            for (int fromNeuron = 0; fromNeuron < fromCount; fromNeuron++)
            {
                for (int toNeuron = 0; toNeuron < toCount; toNeuron++)
                {
                    double sigma = Math.Sqrt(2.0 / (fromCount + toCount));
                    double w = Rnd.NextGaussian() * sigma;
                    network.SetWeight(fromLayer, fromNeuron, toNeuron, w);
                }
            }
        }

        /// <inheritdoc/>
        public override void Randomize(BasicNetwork network)
        {
            for (int i = 0; i < network.Layers.Count - 1; i++)
            {
                RandomizeLayer(network, i);
            }
        }
    }
}
