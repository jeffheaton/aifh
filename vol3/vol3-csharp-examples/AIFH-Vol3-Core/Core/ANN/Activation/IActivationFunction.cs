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

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    ///     This interface allows various activation functions to be used with the neural
    ///     network.Activation functions are applied to the output from each layer of a
    ///     neural network.Activation functions scale the output into the desired range.
    ///     Methods are provided both to process the activation function, as well as the
    ///     derivative of the function.Some training algorithms, particularly back
    ///     propagation, require that it be possible to take the derivative of the
    ///     activation function.
    ///     Not all activation functions support derivatives.If you implement an
    ///     activation function that is not derivable then an exception should be thrown
    ///     inside of the derivativeFunction method implementation.
    ///     Non-derivable activation functions are perfectly valid, they simply cannot be
    ///     used with every training algorithm.
    /// </summary>
    public interface IActivationFunction
    {
        /// <summary>
        ///     Return true if this function has a derivative.
        /// </summary>
        bool HasDerivative { get; }

        /// <summary>
        ///     The params for this activation function.
        /// </summary>
        double[] Params { get; }

        /// <summary>
        ///     The names of the parameters.
        /// </summary>
        string[] ParamNames { get; }

        /// <summary>
        ///     Implements the activation function. The array is modified according to
        ///     the activation function being used.See the class description for more
        ///     specific information on this type of activation function.
        /// </summary>
        /// <param name="d">The input array to the activation function.</param>
        /// <param name="start">The starting index.</param>
        /// <param name="size">The number of values to calculate.</param>
        void ActivationFunction(double[] d, int start, int size);

        /// <summary>
        ///     Calculate the derivative.  For performance reasons two numbers are provided.
        ///     First, the value "b" is simply the number that we would like to calculate
        ///     the derivative of.
        ///     Second, the value "a", which is the value returned by the activation function,
        ///     when presented with "b".
        ///     We use two values because some of the most common activation functions make
        ///     use of the result of the activation function.It is bad for performance to
        ///     calculate this value twice.  Yet, not all derivatives are calculated this way.
        ///     By providing both the value before the activation function is applied ("b"),
        ///     and after the activation function is applied("a"), the class can be constructed
        ///     to use whichever value will be the most efficient.
        /// </summary>
        /// <param name="b">
        ///     The number to calculate the derivative of, the number "before" the
        ///     activation function was applied.
        /// </param>
        /// <param name="a">The number "after" an activation function has been applied.</param>
        /// <returns>The derivative.</returns>
        double DerivativeFunction(double b, double a);

        /// <summary>
        ///     Clone a copy of this activation function.
        /// </summary>
        /// <returns>A cloned copy of this activation function.</returns>
        IActivationFunction Clone();
    }
}