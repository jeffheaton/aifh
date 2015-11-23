
namespace AIFH_Vol3.Core.General.Fns
{
    /// <summary>
    /// A function that implements a radial basis function (RBF).
    /// </summary>
    public interface IFnRBF : IFn
    {
        /// <summary>
        /// Get the center for the specified dimension.
        /// </summary>
        /// <param name="dimension">The dimension.</param>
        /// <returns>The center.</returns>
        double GetCenter(int dimension);

        /// <summary>
        /// Set the center for the specified dimension. 
        /// </summary>
        /// <param name="dimension">The dimension.</param>
        /// <param name="value">The value to set the center.</param>
        void SetCenter(int dimension, double value);

        
        /// <summary>
        /// The dimension count.
        /// </summary>
        int Dimensions { get;  }

        /// <summary>
        /// The width.
        /// </summary>
        double Width { get; set; }

    }
}
