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

using System.Collections;
using System.Text;

namespace AIFH_Vol3_Core.Core.Util
{
    /// <summary>
    ///     Array utilities.
    /// </summary>
    public class ArrayUtil
    {
        /// <summary>
        ///     Return the index of the largest value in a vector.
        /// </summary>
        /// <param name="data">The vector.</param>
        /// <returns>The index of the largest value.</returns>
        public static int IndexOfLargest(double[] data)
        {
            var result = -1;

            for (var i = 0; i < data.Length; i++)
            {
                if (result == -1 || data[i] > data[result])
                    result = i;
            }

            return result;
        }

        public static string List2String(IEnumerable list)
        {
            var result = new StringBuilder("[");
            var first = true;
            foreach (var item in list)
            {
                if (!first)
                {
                    result.Append(",");
                }
                result.Append(item);
                first = false;
            }
            result.Append("]");

            return result.ToString();
        }
    }
}