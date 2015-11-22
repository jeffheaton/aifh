using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3.Core.Randomize;

namespace AIFH_Vol3_Core.Core.Energetic
{
    /// <summary>
    /// Implements a Boltzmann machine.
    /// </summary>
    public class BoltzmannMachine: EnergeticNetwork
    {
        /// <summary>
        /// The current temperature of the neural network. The higher the
	    /// temperature, the more random the network will behave.
        /// </summary>
        private double _temperature;

        /// <summary>
        /// The thresholds.
        /// </summary>
        private double[] _threshold;

        /// <summary>
        /// Count used to internally determine if a neuron is "on".
        /// </summary>
        [NonSerialized]
        private int[] _on;

        /// <summary>
        /// Count used to internally determine if a neuron is "off".
        /// </summary>
        [NonSerialized]
        private int[] _off;

        /// <summary>
        /// The number of cycles to anneal for.
        /// </summary>
        private int _annealCycles = 100;

        /// <summary>
        /// The number of cycles to run the network through before annealing.
        /// </summary>
        private int _runCycles = 1000;

        /// <summary>
        /// Random number generator
        /// </summary>
        private IGenerateRandom _random = new MersenneTwisterGenerateRandom();

        /// <summary>
        /// Default constructors.
        /// </summary>
        public BoltzmannMachine(): base()
        {
        }

        /// <summary>
        /// Construct a Boltzmann machine with the specified number of neurons.
        /// </summary>
        /// <param name="neuronCount">The number of neurons.</param>
        public BoltzmannMachine(int neuronCount): base(neuronCount)
        {
            _threshold = new double[neuronCount];
        }

        /// <summary>
        /// Note: for Boltzmann networks, you will usually want to call the "run"
        /// method to compute the output.
        ///
        /// This method can be used to copy the input data to the current state.A
        /// single iteration is then run, and the new current state is returned.
        /// </summary>
        /// <param name="input">The input pattern.</param>
        /// <returns>The new current state.</returns>
        public double[] Compute(double[] input)
        {
            double[] result = new double[input.Length];
            Array.Copy(input, CurrentState, input.Length);
            Run();
            Array.Copy(CurrentState, result, result.Length);
            return result;
        }


        /// <summary>
        /// Decrease the temperature by the specified amount.
        /// </summary>
        /// <param name="d">The amount to decrease by, for example .8 to change to
        /// 80% of current.</param>
        public void DecreaseTemperature(double d)
        {
            _temperature *= d;
        }



        /// <summary>
        /// Run until state stabilizes.
        /// </summary>
        public void EstablishEquilibrium()
        {
            int count = NeuronCount;

            if (_on == null)
            {
                _on = new int[count];
                _off = new int[count];
            }

            for (int i = 0; i < count; i++)
            {
                _on[i] = 0;
                _off[i] = 0;
            }

            for (int n = 0; n < _runCycles * count; n++)
            {
                Run((int)_random.NextDouble(0, count - 1));
            }
            for (int n = 0; n < _annealCycles * count; n++)
            {
                int i = (int)_random.NextDouble(0, count - 1);
                Run(i);
                if (CurrentState[i] > 0)
                {
                    _on[i]++;
                }
                else
                {
                    _off[i]++;
                }
            }

            for (int i = 0; i < count; i++)
            {
                CurrentState[i] = _on[i] > _off[i] ? 1 : 0;
            }
        }

        /// <summary>
        /// Cycles of annealing.
        /// </summary>
        public int AnnealCycles
        {
            get { return _annealCycles; }
            set { _annealCycles = value; }
        }

        /// <summary>
        /// The number of inputs.
        /// </summary>
        public int InputCount
        {
          get { return NeuronCount; }
        }

        /// <summary>
        /// The number of outputs.
        /// </summary>
        public int OutputCount
        {
            get { return NeuronCount; }
        }

        /// <summary>
        /// The number of cycles to run.
        /// </summary>
        public int RunCycles
        {
            get { return _runCycles; }
            set { _runCycles = value; }
        }

        /// <summary>
        /// The temperature the network is currently operating at.
        /// </summary>
        public double Temperature
        {
            get { return _temperature; }
            set { _temperature = value; }
        }

        /// <summary>
        /// Threshold.
        /// </summary>
        public double[] Threshold
        {
            get { return _threshold; }
        }

        /// <summary>
        /// Run the network for all neurons present.
        /// </summary>
        public void Run()
        {
            int count = NeuronCount;
            for (int i = 0; i < count; i++)
            {
                Run(i);
            }
        }
        
        /// <summary>
        /// Run the network for the specified neuron.
        /// </summary>
        /// <param name="i">The neuron to run for.</param>
        public void Run(int i)
        {
            int j;
            double sum, probability;

            int count = NeuronCount;

            sum = 0;
            for (j = 0; j < count; j++)
            {
                sum += GetWeight(i, j) * ((CurrentState[j] > 0) ? 1 : 0);
            }
            sum -= _threshold[i];
            probability = 1 / (1 + Math.Exp(-sum / _temperature));
            if (_random.NextDouble() <= probability)
            {
                CurrentState[i] = 1.0;
            }
            else
            {
                CurrentState[i] = 0.0;
            }
        }
       


        public IGenerateRandom Random
        {
            get { return _random; }
            set { _random = value; }
        }
        

        /// <inheritdoc/>
        public override double[] LongTermMemory => new double[0];
    }
}
