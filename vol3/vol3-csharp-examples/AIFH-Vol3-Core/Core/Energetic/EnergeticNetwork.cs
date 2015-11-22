using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Randomize;

namespace AIFH_Vol3_Core.Core.Energetic
{
    /// <summary>
    /// The energetic network forms the base class for Hopfield and Boltzmann machines.
    /// </summary>
    public class EnergeticNetwork : IMLMethod
    {

        /// <summary>
        /// The current state of the thermal network.
        /// </summary>
        private double[] _currentState;

        /// <summary>
        /// The weights.
        /// </summary>
        private double[] _weights;

        /// <summary>
        /// The neuron count.
        /// </summary>
        private int _neuronCount;

        /// <summary>
        /// Default constructor.
        /// </summary>
        public EnergeticNetwork()
        {

        }

        /// <summary>
        /// Construct the network with the specified neuron count.
        /// </summary>
        /// <param name="neuronCount">The number of neurons.</param>
        public EnergeticNetwork(int neuronCount)
        {
            _neuronCount = neuronCount;
            _weights = new double[neuronCount*neuronCount];
            _currentState = new double[neuronCount];
        }

        /// <summary>
        /// Add to the specified weight. 
        /// </summary>
        /// <param name="fromNeuron">The from neuron.</param>
        /// <param name="toNeuron">The to neuron.</param>
        /// <param name="value">The value to add.</param>
        public void AddWeight(int fromNeuron, int toNeuron,
            double value)
        {
            int index = (toNeuron*_neuronCount) + fromNeuron;
            if (index >= _weights.Length)
            {
                throw new AIFHError("Out of range: fromNeuron:"
                                    + fromNeuron + ", toNeuron: " + toNeuron);
            }
            _weights[index] += value;
        }

        /// <summary>
        /// Calculate the current energy for the network. The network will
        /// seek to lower this value.
        /// </summary>
        /// <returns>The energy.</returns>
        public double CalculateEnergy()
        {
            double tempE = 0;
            int neuronCount = NeuronCount;

            for (int i = 0; i < neuronCount; i++)
            {
                for (int j = 0; j < neuronCount; j++)
                {
                    if (i != j)
                    {
                        tempE += GetWeight(i, j)*_currentState[i]
                                 *_currentState[j];
                    }
                }
            }
            return -1*tempE/2;

        }

        /// <summary>
        /// Clear any connection weights. 
        /// </summary>
        public void Clear()
        {
            for (int i = 0; i < _weights.Length; i++)
            {
                _weights[i] = 0;
            }
        }

        /// <summary>
        /// The current state of the network.
        /// </summary>
        public double[] CurrentState
        {
            get
            {
                return _currentState;
            }
        }

        /// <summary>
        /// Get the neuron count for the network.
        /// </summary>
        public int NeuronCount
        {
            get { return _neuronCount; }
        }

        /// <summary>
        /// Get a weight.
        /// </summary>
        /// <param name="fromNeuron">The from neuron.</param>
        /// <param name="toNeuron">The to neuron.</param>
        /// <returns>The weight.</returns>
        public double GetWeight(int fromNeuron, int toNeuron)
        {
            int index = (toNeuron * _neuronCount) + fromNeuron;
            return _weights[index];
        }

        /// <summary>
        /// The weights.
        /// </summary>
        public double[] Weights
        {
            get { return _weights; }
        }
        
        /// <summary>
        /// Init the network. 
        /// </summary>
        /// <param name="neuronCount">The neuron count.</param>
        /// <param name="weights">The weights.</param>
        /// <param name="output">The output.</param>
        public void Init(int neuronCount, double[] weights,
                double[] output)
        {
            if (neuronCount != output.Length)
            {
                throw new AIFHError("Neuron count(" + neuronCount
                        + ") must match output count(" + output.Length + ").");
            }

            if ((neuronCount * neuronCount) != weights.Length)
            {
                throw new AIFHError("Weight count(" + weights.Length
                        + ") must be the square of the neuron count(" + neuronCount
                        + ").");
            }

            _neuronCount = neuronCount;
            _weights = weights;
            _currentState = new double[neuronCount];
            Array.Copy(output, _currentState, _currentState.Length);
        }

        /// <summary>
        /// Randomize the weights.
        /// </summary>
        /// <param name="rand">Random number generator.</param>
        public void Reset(IGenerateRandom rand)
        {

            for (int i = 0; i < _currentState.Length; i++)
            {
                _currentState[i] = 0;
            }
            for (int i = 0; i < _weights.Length; i++)
            {
                _weights[i] = 0;
            }
        }
        
        /// <summary>
        /// Set the current state.
        /// </summary>
        /// <param name="s">The current state array.</param>
        public void CopyToCurrentState(double[] s)
        {
            _currentState = new double[s.Length];
            Array.Copy(s, _currentState, s.Length);
        }
        

        /// <summary>
        /// Set the weight. 
        /// </summary>
        /// <param name="fromNeuron">The from neuron.</param>
        /// <param name="toNeuron">The to neuron.</param>
        /// <param name="value">The value.</param>
        public void SetWeight(int fromNeuron, int toNeuron,
                double value)
        {
            int index = (toNeuron * _neuronCount) + fromNeuron;
            _weights[index] = value;
        }

        /// <inheritdoc/>
        public virtual double[] LongTermMemory
        {
            get { return _weights; }
        }
    }
}
