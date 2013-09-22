using AIFH_Vol1.Core.Error;

namespace AIFH_Vol1.Examples.Error
{
    /// <summary>
    /// Simple data holder for actual and ideal.  Just used for this example.
    /// </summary>
    public class DataHolder
    {
        /// <summary>
        /// The actual data.
        /// </summary>
        public double[][] Actual { get; set; }

        /// <summary>
        /// The ideal data, what the actual should have been.
        /// </summary>
        public double[][] Ideal { get; set; }

        /// <summary>
        /// Calculate the error with the specified error calculation.
        /// </summary>
        /// <param name="calc">The error calculation.</param>
        /// <returns>The error.</returns>
        public double CalculateError(IErrorCalculation calc)
        {
            calc.Clear();

            for (int row = 0; row < Actual.Length; row++)
            {
                calc.UpdateError(Actual[row], Ideal[row], 1.0);
            }

            return calc.Calculate();
        }
    }
}
