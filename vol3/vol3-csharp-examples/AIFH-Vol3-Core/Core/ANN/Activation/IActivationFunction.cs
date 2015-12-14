using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    /// This interface allows various activation functions to be used with the neural
    /// network.Activation functions are applied to the output from each layer of a
    /// neural network.Activation functions scale the output into the desired range.
    ///
    /// Methods are provided both to process the activation function, as well as the
    /// derivative of the function.Some training algorithms, particularly back
    /// propagation, require that it be possible to take the derivative of the
    /// activation function.
    ///
    /// Not all activation functions support derivatives.If you implement an
    /// activation function that is not derivable then an exception should be thrown
    /// inside of the derivativeFunction method implementation.
    /// 
    /// Non-derivable activation functions are perfectly valid, they simply cannot be
    /// used with every training algorithm.
    /// </summary>
    public interface IActivationFunction
    {
        /// <summary>
        /// Implements the activation function. The array is modified according to
        /// the activation function being used.See the class description for more
        /// specific information on this type of activation function.
        /// </summary>
        /// <param name="d">The input array to the activation function.</param>
        /// <param name="start">The starting index.</param>
        /// <param name="size">The number of values to calculate.</param>
        void ActivationFunction(double[] d, int start, int size);
        
        /// <summary>
        /// Calculate the derivative.  For performance reasons two numbers are provided.
        /// First, the value "b" is simply the number that we would like to calculate
        /// the derivative of.
        /// 
        /// Second, the value "a", which is the value returned by the activation function,
        /// when presented with "b".  
        /// 
        /// We use two values because some of the most common activation functions make
        /// use of the result of the activation function.It is bad for performance to
        /// calculate this value twice.  Yet, not all derivatives are calculated this way.
        /// By providing both the value before the activation function is applied ("b"), 
        /// and after the activation function is applied("a"), the class can be constructed
        /// to use whichever value will be the most efficient.
        /// </summary>
        /// <param name="b">The number to calculate the derivative of, the number "before" the
        /// activation function was applied.</param>
        /// <param name="a">The number "after" an activation function has been applied.</param>
        /// <returns>The derivative.</returns>
        double DerivativeFunction(double b, double a);

        /// <summary>
        /// Return true if this function has a derivative.
        /// </summary>
        bool HasDerivative { get; }

        /// <summary>
        /// The params for this activation function.
        /// </summary>
        double[] Params { get; }

        /// <summary>
        /// The names of the parameters.
        /// </summary>
        String[] ParamNames { get; }

        /// <summary>
        /// Clone a copy of this activation function.
        /// </summary>
        /// <returns>A cloned copy of this activation function.</returns>
        IActivationFunction Clone();
    }
}
