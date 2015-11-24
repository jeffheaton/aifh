using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    /// Restricted Boltzmann machine, for deep belief neural network.
    /// </summary>
    public class RestrictedBoltzmannMachine
    {
        /// <summary>
        /// The hidden bias.
        /// </summary>
        private double[] _hBias;

        /// <summary>
        /// The visable bias.
        /// </summary>
        private double[] _vBias;

        /// <summary>
        /// The hidden layer that this RBM corresponds to.
        /// </summary>
        private HiddenLayer _layer;

        /// <summary>
        /// The neural network.
        /// </summary>
        private DeepBeliefNetwork _owner;
        
        /// <summary>
        /// Sample a bimodal value with the specified probability.  Returns the count of sampled true values.
        /// </summary>
        /// <param name="n">The number of values to sample.</param>
        /// <param name="p">The probability of true.</param>
        /// <returns>The count of true values.</returns>
        public int binomial(int n, double p)
        {
            if (p < 0 || p > 1) return 0;

            int c = 0;
            double r;

            for (int i = 0; i < n; i++)
            {
                r = _owner.Random.NextDouble();
                if (r < p) c++;
            }

            return c;
        }
        
        /// <summary>
        /// Sigmoid function. 
        /// </summary>
        /// <param name="x">The input.</param>
        /// <returns>The output.</returns>
        public static double Sigmoid(double x)
        {
            return 1.0 / (1.0 + Math.Exp(-x));
        }
        
        /// <summary>
        /// Construct restricted Boltzmann machine. 
        /// </summary>
        /// <param name="theLayer">The layer that this RBM works with.</param>
        public RestrictedBoltzmannMachine(HiddenLayer theLayer)
        {
            _layer = theLayer;
            _owner = theLayer.Owner;
            _hBias = _layer.Bias;
            _vBias = new double[VisibleCount];
        }

        /// <summary>
        /// The visable neuron count.
        /// </summary>
        public int VisibleCount
        {
            get
            {
                return _layer.InputCount;
            }
        }

        /// <summary>
        /// The hidden neuron count.
        /// </summary>
        public int HiddenCount
        {
            get
            {
                return _layer.OutputCount;
            }
        }

        /// <summary>
        /// The hidden layer that goes with this RBM.
        /// </summary>
        public HiddenLayer Layer
        {
            get
            {
                return _layer;
            }
        }

        /// <summary>
        /// Hidden biases.
        /// </summary>
        public double[] BiasH
        {
            get
            {
                return _hBias;
            }
        }

        /// <summary>
        /// Visable biases.
        /// </summary>
        public double[] BiasV
        {
            get
            {
                return _vBias;
            }
        }

        /// <summary>
        /// The network owner.
        /// </summary>
        public DeepBeliefNetwork Owner
        {
            get
            {
                return _owner;
            }
        }
    }
}
