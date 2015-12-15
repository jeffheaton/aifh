using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Util
{
    /// <summary>
    /// Array utilities.
    /// </summary>
    public class ArrayUtil
    {
        /// <summary>
        /// Return the index of the largest value in a vector.
        /// </summary>
        /// <param name="data">The vector.</param>
        /// <returns>The index of the largest value.</returns>
        public static int IndexOfLargest(double[] data)
        {
            int result = -1;

            for (int i = 0; i < data.Length; i++)
            {
                if (result == -1 || data[i] > data[result])
                    result = i;
            }

            return result;
        }
    }
}
