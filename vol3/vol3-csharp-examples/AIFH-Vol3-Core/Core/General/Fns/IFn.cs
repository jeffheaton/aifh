
namespace AIFH_Vol3.Core.General.Fns
{
    /// <summary>
    /// A function.  Returns a single scalar variable, accepts a vector of x.
    /// </summary>
    public interface IFn
    {
        /// <summary>
        /// Evaluate the function.
        /// </summary>
        /// <param name="x">A vector input.</param>
        /// <returns>The output from the function.</returns>
        double Evaluate(double[] x);
    }
}
