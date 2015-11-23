
namespace AIFH_Vol3.Core.Error
{
    /// <summary>
    /// Calculates the error as the average of the sum of the squared differences between the actual and ideal vectors.
    /// This is the most commonly used error calculation technique in this book.
    ///
    /// http://www.heatonresearch.com/wiki/Mean_Square_Error
    /// </summary>
    public class ErrorCalculationMSE : AbstractErrorCalculation
    {
        /// <summary>
        /// Calculate the error with MSE.
        /// </summary>
        /// <returns>The current error.</returns>
        public override double Calculate()
        {
            if (SetSize == 0)
            {
                return double.PositiveInfinity;
            }
            return GlobalError / SetSize;

        }
    }
}
