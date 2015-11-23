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
    public class TrainHopfieldHebbian
    {
        /// <summary>
        /// The network to train.
        /// </summary>
        private HopfieldNetwork _network;

        /// <summary>
        /// The summation matrix.
        /// </summary>
        private double[][] _sumMatrix;

        /// <summary>
        /// The count of patterns.
        /// </summary>
        private int _patternCount;
        
        /// <summary>
        /// Construct the trainer. 
        /// </summary>
        /// <param name="theNetwork">The network to train.</param>
        public TrainHopfieldHebbian(HopfieldNetwork theNetwork)
        {
            _network = theNetwork;
            _sumMatrix = AIFH.Alloc2D<double>(_network.InputCount,_network.InputCount);
        }
        
        /// <summary>
        /// Add a pattern to train.
        /// </summary>
        /// <param name="pattern">The pattern to train.</param>
        public void AddPattern(double[] pattern)
        {
            for (int i = 0; i < _sumMatrix.Length; i++)
            {
                for (int j = 0; j < _sumMatrix.Length; j++)
                {
                    if (i == j)
                    {
                        _sumMatrix[i][j] = 0;
                    }
                    else
                    {
                        _sumMatrix[i][j] += pattern[i] * pattern[j];
                    }
                }
            }
            _patternCount++;
        }

        /// <summary>
        /// Learn the added patterns.
        /// </summary>
        public void Learn()
        {
            if (_patternCount == 0)
            {
                throw new AIFHError("Please add a pattern before learning.  Nothing to learn.");
            }

            for (int i = 0; i < _sumMatrix.Length; i++)
            {
                for (int j = 0; j < _sumMatrix.Length; j++)
                {
                    _network.SetWeight(i, j, _sumMatrix[i][j] / _patternCount);
                }
            }
        }
    }
}
