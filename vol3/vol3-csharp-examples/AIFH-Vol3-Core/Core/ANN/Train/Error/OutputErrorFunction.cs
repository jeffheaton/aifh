using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3_Core.Core.ANN.Activation;

namespace AIFH_Vol3_Core.Core.ANN.Train.Error
{
    /// <summary>
    /// A very simple quadratic error function.  It is usually better to use the cross entropy error function.
    /// </summary>
    public class OutputErrorFunction : IErrorFunction
    {
        /// <inheritdoc/>
        public void CalculateError(IActivationFunction af, double[] b, double[] a,
                                   double[] ideal, double[] actual, double[] error, double derivShift,
                                   double significance)
        {

            for (int i = 0; i < actual.Length; i++)
            {
                double deriv = af.DerivativeFunction(b[i], a[i]) + derivShift;
                error[i] = ((ideal[i] - actual[i]) * significance) * deriv;
            }
        }
    }
}
