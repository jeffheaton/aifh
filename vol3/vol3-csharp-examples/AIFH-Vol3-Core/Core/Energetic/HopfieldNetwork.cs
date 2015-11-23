using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Energetic
{
    /// <summary>
    /// Implements a Hopfield network.
    /// </summary>
    public class HopfieldNetwork: EnergeticNetwork
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public HopfieldNetwork()
        {

        }
        
        /// <summary>
        /// Construct a Hopfield with the specified neuron count.
        /// </summary>
        /// <param name="neuronCount">The neuron count.</param>
        public HopfieldNetwork(int neuronCount): base(neuronCount)
        {

        }
        
        /// <summary>
        /// Note: for Hopfield networks, you will usually want to call the "run"
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

            for (int i = 0; i < CurrentState.Length; i++)
            {
                result[i] = ActivationFunction(CurrentState[i]);
            }
            Array.Copy(CurrentState, 0, result, 0, result.Length);
            return result;
        }

        public double ActivationFunction(double d)
        {
            return (d > 0) ? 1 : 0;
        }

        /// <inheritdoc/>
        public int InputCount
        {
            get { return NeuronCount; }
        }

        /// <inheritdoc/>
        public int OutputCount
        {
            get { return base.NeuronCount; }
        }

        /// <summary>
        /// Perform one Hopfield iteration.
        /// </summary>
        public void Run()
        {

            for (int toNeuron = 0; toNeuron < NeuronCount; toNeuron++)
            {
                double sum = 0;
                for (int fromNeuron = 0; fromNeuron < NeuronCount; fromNeuron++)
                {
                    sum += CurrentState[fromNeuron]
                            * GetWeight(fromNeuron, toNeuron);
                }
                CurrentState[toNeuron] = ActivationFunction(sum);
            }
        }


        /// <summary>
        /// Run the network until it becomes stable and does not change from more
        /// runs.
        /// </summary>
        /// <param name="max">The maximum number of cycles to run before giving up.</param>
        /// <returns>The number of cycles that were run.</returns>
        public int RunUntilStable(int max)
        {
            bool done = false;
            string lastStateStr = CurrentState.ToString();
            string currentStateStr = lastStateStr;

            int cycle = 0;
            do
            {
                Run();
                cycle++;

                lastStateStr = CurrentState.ToString();

                if (!currentStateStr.Equals(lastStateStr))
                {
                    if (cycle > max)
                    {
                        done = true;
                    }
                }
                else
                {
                    done = true;
                }

                currentStateStr = lastStateStr;

            } while (!done);

            return cycle;
        }

        public double Energy()
        {
            double t = 0;

            // Calculate first term
            double a = 0;
            for (int i = 0; i < InputCount; i++)
            {
                for (int j = 0; j < OutputCount; j++)
                {
                    a += this.GetWeight(i, j) * this.CurrentState[i] * this.CurrentState[j];
                }
            }
            a *= -0.5;

            // Calculate second term
            double b = 0;
            for (int i = 0; i < InputCount; i++)
            {
                b += this.CurrentState[i] * t;
            }

            return a + b;
        }
    }
}
