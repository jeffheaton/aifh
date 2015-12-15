using AIFH_Vol3_Core.Core.ANN.Activation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Train.Error
{
    /// <summary>
    /// Implements a cross entropy error function.  This can be used with backpropagation to
    /// sometimes provide better performance than the standard linear error function.
    ///
    /// De Boer, Pieter-Tjerk, et al. "A tutorial on the cross-entropy method." Annals of operations
    /// research 134.1 (2005): 19-67.
    /// </summary>
    public class CrossEntropyErrorFunction:IErrorFunction
    {
        /// <inheritdoc/>
        public void CalculateError(IActivationFunction af, double[] b, double[] a,
                               double[] ideal, double[] actual, double[] error, double derivShift,
                               double significance)
        {

            for (int i = 0; i < actual.Length; i++)
            {
                error[i] = (ideal[i] - actual[i]) * significance;
            }
        }
    }
}
