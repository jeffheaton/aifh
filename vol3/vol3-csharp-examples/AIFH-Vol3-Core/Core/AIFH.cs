
namespace AIFH_Vol3.Core
{
    /// <summary>
    /// Global constants for AIFH.
    /// </summary>
    public class AIFH
    {
        /// <summary>
        /// The default precision.
        /// </summary>
        public const double DefaultPrecision = 0.0000001;

        /// <summary>
        /// Private constructor.
        /// </summary>
        private AIFH()
        {

        }

        /// <summary>
        /// Allocate a 2D array.
        /// </summary>
        /// <typeparam name="T">The type to allocate.</typeparam>
        /// <param name="rows">Rows</param>
        /// <param name="cols">Columns</param>
        /// <returns>The array.</returns>
        public static T[][] Alloc2D<T>(int rows, int cols)
        {
            T[][] result = new T[rows][];
            for (int i = 0; i < rows; i++)
            {
                result[i] = new T[cols];
            }
            return result;
        }
    }
}
