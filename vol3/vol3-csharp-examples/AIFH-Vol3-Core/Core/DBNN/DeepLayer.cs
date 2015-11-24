using AIFH_Vol3.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    /// A layer for a deep belief neural network.
    /// </summary>
    public class DeepLayer
    {
        /// <summary>
        /// The weights of this layer.
        /// </summary>
        private double[][] _weights;

        /// <summary>
        /// The biases.
        /// </summary>
        private double[] _bias;

        /// <summary>
        /// The network that owns this layer.
        /// </summary>
        private DeepBeliefNetwork _owner;
        
        /// <summary>
        /// Construct the layer. 
        /// </summary>
        /// <param name="theOwner">The network that owns this layer.</param>
        /// <param name="inputCount">The input count for this layer.</param>
        /// <param name="outputCount">The output count for this layer.</param>
        public DeepLayer(DeepBeliefNetwork theOwner, int inputCount, int outputCount)
        {
            _weights = AIFH.Alloc2D<double>(outputCount,inputCount);
            _bias = new double[outputCount];
            _owner = theOwner;
        }
        
        /// <summary>
        /// Used to calculate the softmax for this layer. 
        /// </summary>
        /// <param name="x">The input to the softmax.</param>
        public void Softmax(double[] x)
        {
            double max = 0.0;
            double sum = 0.0;

            foreach (double aX in x)
            {
                if (max < aX)
                {
                    max = aX;
                }
            }

            for (int i = 0; i < x.Length; i++)
            {
                x[i] = Math.Exp(x[i] - max);
                sum += x[i];
            }

            for (int i = 0; i < x.Length; i++)
            {
                x[i] /= sum;
            }
        }

        /// <summary>
        /// The input count.
        /// </summary>
        public virtual int InputCount
        {
            get { return _weights[0].Length; }
        }

        /// <summary>
        /// The output count.
        /// </summary>
        public virtual int OutputCount
        { 
            get { return _weights.Length;
            }
        }

        /// <summary>
        /// The weights.
        /// </summary>
        public double[][] Weights
        {
            get { return _weights; }
        }

        /// <summary>
        /// The biases.
        /// </summary>
        public double[] Bias
        {
            get { return _bias; }
        }

        /// <summary>
        /// The network.
        /// </summary>
        public DeepBeliefNetwork Owner
        {
            get { return _owner; }
        }
    }
}
