
using System.Globalization;
using System.Linq;

namespace AIFH_Vol3.Core.General
{
    /// <summary>
    /// Some vector utilities.
    /// </summary>
    public class VectorUtil
    {
        /// <summary>
        /// Private constructor.
        /// </summary>
        private VectorUtil()
        {

        }

        /// <summary>
        /// Return the index that has the max value. 
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
        /// Create a comma separated list of a double array.
        /// </summary>
        /// <param name="arr"></param>
        /// <returns></returns>
        public static string DoubleArrayToString(double[] arr)
        {
            return string.Join(",", arr.Select(p => p.ToString(CultureInfo.InvariantCulture)).ToArray());
        }
    }
}
