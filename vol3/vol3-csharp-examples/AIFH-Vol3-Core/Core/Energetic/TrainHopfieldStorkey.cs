using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3.Core;

namespace AIFH_Vol3_Core.Core.Energetic
{
    /// <summary>
    /// Train the Hopfield network using a Hebbian algorithm.
    /// For more info: https://en.wikipedia.org/wiki/Hopfield_network
    /// </summary>
    public class TrainHopfieldStorkey
    {
        /// <summary>
        /// The network to train.
        /// </summary>
        private HopfieldNetwork _network;

        /// <summary>
        // The summation matrix.
        /// </summary>
        private double[][] _sumMatrix;

        /// <summary>
        /// Construct the trainer.
        /// </summary>
        /// <param name="theNetwork">The network to train.</param>
        public TrainHopfieldStorkey(HopfieldNetwork theNetwork)
        {
            _network = theNetwork;
            _sumMatrix = AIFH.Alloc2D<double>(_network.InputCount,_network.InputCount);
        }

        /// <summary>
        /// Calculate the local field needed by training. 
        /// </summary>
        /// <param name="i">The neuron.</param>
        /// <param name="pattern">The pattern.</param>
        /// <returns>The local field value.</returns>
        private double CalculateLocalField(int i, double[] pattern)
        {
            double sum = 0;
            for (int k = 0; k < _network.InputCount; k++)
            {
                if (k != i)
                {
                    sum += _network.GetWeight(i, k) * pattern[k];
                }
            }
            return sum;
        }
        
        /// <summary>
        /// Add a pattern for training. 
        /// </summary>
        /// <param name="pattern">The pattern to add.</param>
        public void AddPattern(double[] pattern)
        {
            for (int i = 0; i < _sumMatrix.Length; i++)
            {
                for (int j = 0; j < _sumMatrix.Length; j++)
                {
                    _sumMatrix[i][j] = 0;
                }
            }

            double n = _network.InputCount;
            for (int i = 0; i < _sumMatrix.Length; i++)
            {
                for (int j = 0; j < _sumMatrix.Length; j++)
                {
                    double t1 = (pattern[i] * pattern[j]) / n;
                    double t2 = (pattern[i] * CalculateLocalField(j, pattern)) / n;
                    double t3 = (pattern[j] * CalculateLocalField(i, pattern)) / n;
                    double d = t1 - t2 - t3;
                    _sumMatrix[i][j] += d;
                }
            }

            for (int i = 0; i < _sumMatrix.Length; i++)
            {
                for (int j = 0; j < _sumMatrix.Length; j++)
                {
                    _network.SetWeight(i, j, _network.GetWeight(i, j) + _sumMatrix[i][j]);
                }
            }
        }


    }
}
