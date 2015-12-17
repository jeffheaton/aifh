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

using System.Globalization;
using System.Linq;

namespace AIFH_Vol3.Core.General
{
    /// <summary>
    ///     Some vector utilities.
    /// </summary>
    public class VectorUtil
    {
        /// <summary>
        ///     Private constructor.
        /// </summary>
        private VectorUtil()
        {
        }

        /// <summary>
        ///     Return the index that has the max value.
        /// </summary>
        /// <param name="a">The vector.</param>
        /// <returns>The index.</returns>
        public static int MaxIndex(double[] a)
        {
            var result = -1;
            var max = double.NegativeInfinity;

            for (var i = 0; i < a.Length; i++)
            {
                if (a[i] > max)
                {
                    max = a[i];
                    result = i;
                }
            }

            return result;
        }

        /// <summary>
        ///     Create a comma separated list of a double array.
        /// </summary>
        /// <param name="arr"></param>
        /// <returns></returns>
        public static string DoubleArrayToString(double[] arr)
        {
            return string.Join(",", arr.Select(p => p.ToString(CultureInfo.InvariantCulture)).ToArray());
        }
    }
}