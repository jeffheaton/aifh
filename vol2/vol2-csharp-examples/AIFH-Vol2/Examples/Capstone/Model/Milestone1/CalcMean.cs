namespace AIFH_Vol2.Examples.Capstone.Model.Milestone1
{
    /// <summary>
    /// Calculate the mean of a series of doubles.
    /// </summary>
    public class CalcMean
    {
        /// <summary>
        ///     How many values have we encountered so far.
        /// </summary>
        private int _count;

        /// <summary>
        ///     What is the sum of values.
        /// </summary>
        private double _sum;

        /// <summary>
        ///     Update mean for a new value.
        /// </summary>
        /// <param name="d">The next value.</param>
        public void Update(double d)
        {
            _sum += d;
            _count++;
        }

        /// <summary>
        ///     The calculated mean.
        /// </summary>
        /// <returns>The calculated mean.</returns>
        public double Calculate()
        {
            return _sum/_count;
        }
    }
}