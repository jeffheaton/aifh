
namespace AIFH_Vol3.Core.Error
{
    /// <summary>
    /// The sum of squares method (SSE) measures the error as the sum of the squared difference of each 
    /// vector element.
    ///
    /// http://www.heatonresearch.com/wiki/Sum_of_Squares_Error
    /// </summary>
    public class ErrorCalculationSSE : AbstractErrorCalculation
    {
        /// <summary>
        /// Calculate the error with ESS.
        /// </summary>
        /// <returns>The current error.</returns>
        public override double Calculate()
        {
            if (SetSize == 0)
            {
                return double.PositiveInfinity;
            }
            return GlobalError;

        }
    }
}
