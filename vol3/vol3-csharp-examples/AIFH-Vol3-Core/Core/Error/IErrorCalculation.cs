
namespace AIFH_Vol3.Core.Error
{
    /// <summary>
    /// An error calculation metric calculates the difference between two vector sets.  One vector set will be the ideal
    /// expected output from a Machine Learning Algorithm.  The other vector set is the actual output.  Training a
    /// Machine Learning Algorithm typically involves minimizing this error.
    ///
    /// Error calculation metrics are very similar to distance metrics.  However, an error calculation metric operates over
    /// a set of vectors, whereas a distance metric operates over just two vectors.
    /// </summary>
    public interface IErrorCalculation
    {
        /// <summary>
        /// Called to update for each number that should be checked.
        /// </summary>
        /// <param name="actual">The actual number.</param>
        /// <param name="ideal">The ideal number.</param>
        /// <param name="significance">The significance, 0 none, 1.0 full.</param>
        void UpdateError(double[] actual, double[] ideal, double significance);

        /// <summary>
        /// Update the error with single values.s 
        /// </summary>
        /// <param name="actual">The actual value.</param>
        /// <param name="ideal">The ideal value.</param>
        void UpdateError(double actual, double ideal);

        /// <summary>
        /// Calculate the error with MSE. 
        /// </summary>
        /// <returns>The current error for the neural network.</returns>
        double Calculate();

        /// <summary>
        /// Clear the error calculation and start over.
        /// </summary>
        void Clear();

        /// <summary>
        /// The total size of the set (vector size times number of vectors).
        /// </summary>
        int SetSize { get; }
    }
}
