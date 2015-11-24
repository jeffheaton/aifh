using AIFH_Vol3.Core;
using AIFH_Vol3.Core.General.Fns;

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    /// A neighborhood function based on an RBF function.
    /// </summary>
    public class NeighborhoodRBF1D : INeighborhoodFunction
    {
        /// <summary>
        /// The radial basis function (RBF) to use to calculate the training falloff
	    /// from the best neuron.
        /// </summary>
        private IFnRBF _radial;

        private double[] _params = new double[2];
        
        /// <summary>
        /// Construct the neighborhood function with the specified radial function.
        /// Generally this will be a Gaussian function but any RBF should do.
        /// </summary>
        /// <param name="radial">The radial basis function to use.</param>
        public NeighborhoodRBF1D(IFnRBF radial)
        {
            _radial = radial;
        }
        
        /// <summary>
        /// Construct a 1d neighborhood function. 
        /// </summary>
        /// <param name="type">The RBF type to use.</param>
        public NeighborhoodRBF1D(RBFEnum type)
        {

            switch (type)
            {
                case RBFEnum.Gaussian:
                    _radial = new GaussianFunction(1, _params, 0);
                    break;
                case RBFEnum.InverseMultiquadric:
                    _radial = new InverseMultiquadricFunction(1, _params, 0);
                    break;
                case RBFEnum.Multiquadric:
                    _radial = new MultiquadricFunction(1, _params, 0);
                    break;
                case RBFEnum.MexicanHat:
                    _radial = new MexicanHatFunction(1, _params, 0);
                    break;
                default:
                    throw new AIFHError("Unknown RBF type: " + type.ToString());
            }

            _radial.Width = 1.0;
        }
        
        /// <summary>
        /// Determine how much the current neuron should be affected by training
        /// based on its proximity to the winning neuron.
        /// </summary>
        /// <param name="currentNeuron"> THe current neuron being evaluated.</param>
        /// <param name="bestNeuron">The winning neuron.</param>
        /// <returns>The ratio for this neuron's adjustment.</returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            double[] d = new double[1];
            d[0] = currentNeuron - bestNeuron;
            return _radial.Evaluate(d);
        }

        /// <summary>
        /// The radius.
        /// </summary>
        public double Radius
        {
            get { return _radial.Width; }
            set { _radial.Width = value; }
        }

    }
}
