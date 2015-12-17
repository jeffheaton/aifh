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

using AIFH_Vol3_Core.Core.ANN.Activation;

namespace AIFH_Vol3_Core.Core.ANN.Train.Error
{
    /// <summary>
    ///     An error function.  This is used to calculate the errors for the
    ///     output layer during propagation training.
    /// </summary>
    public interface IErrorFunction
    {
        /// <summary>
        ///     Calculate the error.
        /// </summary>
        /// <param name="af">The activation function.</param>
        /// <param name="b">The output, before the activation function.</param>
        /// <param name="a">The output, after the activation function.</param>
        /// <param name="ideal">The idea/expected output.</param>
        /// <param name="actual">The actual output.</param>
        /// <param name="error">Error vector (output)</param>
        /// <param name="derivShift">Any derivative shift to apply (usually 0.0), used to implement flat-spot problem shift.</param>
        /// <param name="significance">The significance weight (usually 1.0)</param>
        void CalculateError(IActivationFunction af, double[] b, double[] a,
            double[] ideal, double[] actual, double[] error, double derivShift,
            double significance);
    }
}