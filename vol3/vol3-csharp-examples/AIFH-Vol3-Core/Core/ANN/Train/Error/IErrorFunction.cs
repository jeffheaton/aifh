using AIFH_Vol3_Core.Core.ANN.Activation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Train.Error
{
    /// <summary>
    /// An error function.  This is used to calculate the errors for the
    /// output layer during propagation training.
    /// </summary>
    public interface IErrorFunction
    {
        /**
    * Calculate the error.
    * @param af The activation function.
    * @param b The output, before the activation function.
    * @param a The output, after the activation function.
    * @param ideal The idea/expected output.
    * @param actual The actual output.
    * @param error Error vector (output)
    * @param derivShift Any derivative shift to apply (usually 0.0), used to implement flat-spot problem shift.
    * @param significance The significance weight (usually 1.0)
    */
        void CalculateError(IActivationFunction af, double[] b, double[] a,
                                   double[] ideal, double[] actual, double[] error, double derivShift,
                                   double significance);
    }
}
