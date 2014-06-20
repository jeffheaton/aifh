using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning
{
    /// <summary>
    /// This interface defines a Machine Learning Method.  A MLMethod is an
    /// algorithm that accepts data and provides some sort of insight into it.
    /// This could be a neural network, support vector machine, clustering
    /// algorithm, or something else entirely.
    /// </summary>
    public interface IMLMethod
    {
        /// <summary>
        /// The long term memory for the algorithm.  This is usually weights or other coefficients.
        /// </summary>
        double[] LongTermMemory { get; }
    }
}
