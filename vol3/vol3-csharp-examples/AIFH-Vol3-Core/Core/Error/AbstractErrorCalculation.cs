
namespace AIFH_Vol3.Core.Error
{
    /// <summary>
    /// An abstract error calculation class that provides some basic functionality.
    /// </summary>
    public abstract class AbstractErrorCalculation : IErrorCalculation
    {
        /// <summary>
        /// The overall error.
        /// </summary>
        protected double GlobalError;

        /// <summary>
        /// The size of a set.
        /// </summary>
        protected int GlobalSetSize;

        /// <inheritdoc/>
        public void UpdateError(double[] actual, double[] ideal, double significance)
        {
            for (int i = 0; i < actual.Length; i++)
            {
                double delta = (ideal[i] - actual[i]) * significance;

                GlobalError += delta * delta;
            }

            GlobalSetSize += ideal.Length;
        }

        /// <inheritdoc/>
        public void UpdateError(double actual, double ideal)
        {
            double delta = ideal - actual;
            GlobalError += delta * delta;
            GlobalSetSize++;

        }

        /// <inheritdoc/>
        public abstract double Calculate();

        /// <inheritdoc/>
        public void Clear()
        {
            GlobalError = GlobalSetSize = 0;
        }

        /// <inheritdoc/>
        public int SetSize
        {
            get
            {
                return GlobalSetSize;
            }
        }
    }
}
