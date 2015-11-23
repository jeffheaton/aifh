

using System;

namespace AIFH_Vol3.Core.Error
{
    /// <summary>
    /// Calculates the error as the square root of the average of the sum of the squared differences between 
    /// the actual and ideal vectors.
    ///
    /// http://www.heatonresearch.com/wiki/Root_Mean_Square_Error
    /// </summary>
    public class ErrorCalculationRMS : AbstractErrorCalculation
    {
        /// <summary>
        /// Calculate the error with RMS.
        /// </summary>
        /// <returns>The current error for the neural network.</returns>
        public override double Calculate()
        {
            if (SetSize == 0)
            {
                return double.PositiveInfinity;
            }
            return Math.Sqrt(GlobalError / SetSize);
        }
    }
}
