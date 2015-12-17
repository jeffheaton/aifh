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
using System.Collections.Generic;
using System.Text;

namespace AIFH_Vol3.Core.General.Data
{
    /// <summary>
    ///     This class is used to store both the input and ideal vectors for a single item of training data.
    ///     A label can also be applied.
    /// </summary>
    public class BasicData
    {
        /// <summary>
        ///     The ideal (or expected output) vector.
        /// </summary>
        private readonly double[] _ideal;

        /// <summary>
        ///     The input vector.
        /// </summary>
        private readonly double[] _input;

        /// <summary>
        ///     Construct an empty unsupervised element.  An unsupervised element does not have an expected output.
        /// </summary>
        /// <param name="theInputDimensions">The number of dimensions.</param>
        public BasicData(int theInputDimensions)
            : this(theInputDimensions, 0, null)
        {
        }

        /// <summary>
        ///     Construct an empty supervised element.  A supervised element has both input an ideal.
        /// </summary>
        /// <param name="theInputDimensions">The dimensions for the input vector.</param>
        /// <param name="theIdealDimensions">The dimensions for the ideal vector.</param>
        public BasicData(int theInputDimensions, int theIdealDimensions)
            : this(theInputDimensions, theIdealDimensions, null)
        {
        }

        /// <summary>
        ///     Construct an object with the specified dimensions and label.
        /// </summary>
        /// <param name="theInputDimensions">The input dimensions.</param>
        /// <param name="theIdealDimensions">The ideal dimensions.</param>
        /// <param name="theLabel">The label.</param>
        public BasicData(int theInputDimensions, int theIdealDimensions, string theLabel)
        {
            Label = theLabel;
            _input = new double[theInputDimensions];
            _ideal = new double[theIdealDimensions];
        }

        /// <summary>
        ///     Construct a supervised element, with a label.
        /// </summary>
        /// <param name="theInputData">The input data vector.</param>
        /// <param name="theIdealData">The ideal data vector.</param>
        /// <param name="theLabel">The label.</param>
        public BasicData(double[] theInputData, double[] theIdealData, string theLabel)
        {
            Label = theLabel;
            _input = theInputData;
            _ideal = theIdealData;
        }

        /// <summary>
        ///     Construct an unsupervised element, with a label.
        /// </summary>
        /// <param name="theInputData">The input vector.</param>
        /// <param name="theLabel">The label.</param>
        public BasicData(double[] theInputData, string theLabel)
            : this(theInputData, new double[0], theLabel)
        {
        }

        /// <summary>
        ///     Construct an unsupervised element, without a label.
        /// </summary>
        /// <param name="theInputData">The input vector.</param>
        public BasicData(double[] theInputData)
            : this(theInputData, null)
        {
        }

        /// <summary>
        ///     A label, that can be used to tag this element.
        /// </summary>
        public string Label { get; set; }

        /// <summary>
        ///     The input vector.
        /// </summary>
        public double[] Input
        {
            get { return _input; }
        }

        /// <summary>
        ///     The ideal vector.
        /// </summary>
        public double[] Ideal
        {
            get { return _ideal; }
        }


        /// <inheritdoc />
        public override string ToString()
        {
            var result = new StringBuilder();
            result.Append("[BasicData: input:");
            result.Append(VectorUtil.DoubleArrayToString(_input));
            result.Append(", ideal:");
            result.Append(VectorUtil.DoubleArrayToString(_ideal));
            result.Append(", label:");
            result.Append(Label);
            result.Append("]");

            return result.ToString();
        }

        /// <summary>
        ///     Convert two 2D arrays into a List of BasicData elements.  One array holds input and the other ideal
        ///     vectors.
        /// </summary>
        /// <param name="inputData">An array of input vectors.</param>
        /// <param name="idealData">An array of ideal vectors.</param>
        /// <returns>A list of BasicData elements.</returns>
        public static IList<BasicData> ConvertArrays(double[][] inputData, double[][] idealData)
        {
            // create the list
            var result = new List<BasicData>();

            // get the lengths
            var inputCount = inputData[0].Length;
            var idealCount = idealData[0].Length;

            // build the list
            for (var row = 0; row < inputData.Length; row++)
            {
                var dataRow = new BasicData(inputCount, idealCount);
                Array.Copy(inputData[row], dataRow.Input, inputCount);
                Array.Copy(idealData[row], dataRow.Ideal, idealCount);
                result.Add(dataRow);
            }

            return result;
        }
    }
}