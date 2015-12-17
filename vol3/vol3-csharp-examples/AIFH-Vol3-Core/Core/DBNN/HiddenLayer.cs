// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//

using System;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    ///     A hidden layer for a DBNN.  This is based on a restricted Boltzmann machine.
    /// </summary>
    public class HiddenLayer : DeepLayer
    {
        /// <summary>
        ///     Create a hidden layer for a DBNN.
        /// </summary>
        /// <param name="theOwner">The DBNN that this layer belongs to.</param>
        /// <param name="theInputCount">The number of visible units, the input.</param>
        /// <param name="theOutputCount">The number of hidden units, the output.</param>
        public HiddenLayer(DeepBeliefNetwork theOwner, int theInputCount, int theOutputCount) :
            base(theOwner, theInputCount, theOutputCount)
        {
        }

        /// <summary>
        ///     The number of input (visible) units.
        /// </summary>
        public override int InputCount
        {
            get { return Weights[0].Length; }
        }

        /// <summary>
        ///     The number of output (visible) units.
        /// </summary>
        public override int OutputCount
        {
            get { return Weights.Length; }
        }

        /// <summary>
        ///     Sample n times at probability p and return the count of how many samples were 1 (true).
        /// </summary>
        /// <param name="n">The number of samples needed.</param>
        /// <param name="p">The probability of choosing 1 (true).</param>
        /// <returns>The count of how many 1 (true)'s were sampled.</returns>
        public int Binomial(int n, double p)
        {
            if (p < 0 || p > 1) return 0;

            var c = 0;
            double r;

            for (var i = 0; i < n; i++)
            {
                r = Owner.Random.NextDouble();
                if (r < p) c++;
            }

            return c;
        }

        /// <summary>
        ///     Compute the sigmoid (logisitic) for x.
        /// </summary>
        /// <param name="x">The value to compute for.</param>
        /// <returns>The result.</returns>
        public static double Sigmoid(double x)
        {
            return 1.0/(1.0 + Math.Exp(-x));
        }

        /// <summary>
        ///     Calculate the sigmoid output for this layer.
        /// </summary>
        /// <param name="input">The input values for this layer's visable.</param>
        /// <param name="w">The weights for this layer.</param>
        /// <param name="b">The bias value for this layer.</param>
        /// <returns>The hidden values for this layer, the output.</returns>
        public double Output(double[] input, double[] w, double b)
        {
            var linearOutput = 0.0;

            // First calculate the linear output.  Similar to linear regression.
            for (var j = 0; j < InputCount; j++)
            {
                linearOutput += w[j]*input[j];
            }
            linearOutput += b;

            // Now return the signoid of the linear sum.
            return Sigmoid(linearOutput);
        }

        /// <summary>
        ///     Sample the hidden (h) output values, given the (v) input values.  This is different than the output method
        ///     in that we are actually sampling discrete(0 or 1) values.
        /// </summary>
        /// <param name="v">The visible units.</param>
        /// <param name="h">The hidden units, the count of how many times a true (1) was sampled.</param>
        public void SampleHgivenV(double[] v, double[] h)
        {
            for (var i = 0; i < OutputCount; i++)
            {
                h[i] = Binomial(1, Output(v, Weights[i], Bias[i]));
            }
        }
    }
}