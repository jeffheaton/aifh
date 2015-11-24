using System;
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.General.Fns;

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    /// Implements a multi-dimensional RBF neighborhood function. 
    /// </summary>
    public class NeighborhoodRBF : INeighborhoodFunction
    {
        /// <summary>
        /// The radial basis function to use.
        /// </summary>
        private IFnRBF _rbf;

        /// <summary>
        /// The size of each dimension.
        /// </summary>
        private readonly int[] _size;

        /// <summary>
        /// The displacement of each dimension, when mapping the dimensions
        /// to a 1d array.
        /// </summary>
        private int[] _displacement;

        private double[] _params;

        private bool _hexagon;

        public readonly double SQ75 = Math.Sqrt(0.75);
        
        /// <summary>
        /// Construct a 2d neighborhood function based on the sizes for the
        /// x and y dimensions.
        /// </summary>
        /// <param name="type">The RBF type to use.</param>
        /// <param name="x">The size of the x-dimension.</param>
        /// <param name="y">The size of the y-dimension.</param>
        public NeighborhoodRBF(RBFEnum type, int x, int y)
        {
            int[] size = new int[2];
            size[0] = x;
            size[1] = y;

            _params = new double[3];

            switch (type)
            {
                case RBFEnum.Gaussian:
                    _rbf = new GaussianFunction(2, _params, 0);
                    break;
                case RBFEnum.InverseMultiquadric:
                    _rbf = new InverseMultiquadricFunction(2, _params, 0);
                    break;
                case RBFEnum.Multiquadric:
                    _rbf = new MultiquadricFunction(2, _params, 0);
                    break;
                case RBFEnum.MexicanHat:
                    _rbf = new MexicanHatFunction(2, _params, 0);
                    break;
            }

            _rbf.Width = 1;

            _size = size;

            CalculateDisplacement();
        }
        
        /// <summary>
        /// Construct a multi-dimensional neighborhood function. 
        /// </summary>
        /// <param name="size">The sizes of each dimension.</param>
        /// <param name="type">The RBF type to use.</param>
        public NeighborhoodRBF(int[] size, RBFEnum type)
        {
            _params = new double[size.Length];

            switch (type)
            {
                case RBFEnum.Gaussian:
                    _rbf = new GaussianFunction(size.Length, _params, 0);
                    break;
                case RBFEnum.InverseMultiquadric:
                    _rbf = new InverseMultiquadricFunction(size.Length, _params, 0);
                    break;
                case RBFEnum.Multiquadric:
                    _rbf = new MultiquadricFunction(size.Length, _params, 0);
                    break;
                case RBFEnum.MexicanHat:
                    _rbf = new MexicanHatFunction(size.Length, _params, 0);
                    break;
            }
            _size = size;
            CalculateDisplacement();
        }

        /// <summary>
        /// Calculate all of the displacement values.
        /// </summary>
        private void CalculateDisplacement()
        {
            _displacement = new int[_size.Length];
            for (int i = 0; i < _size.Length; i++)
            {
                int value;

                if (i == 0)
                {
                    value = 0;
                }
                else if (i == 1)
                {
                    value = _size[0];
                }
                else
                {
                    value = _displacement[i - 1] * _size[i - 1];
                }

                _displacement[i] = value;
            }
        }
        
        /// <summary>
        /// Calculate the value for the multi RBF function. 
        /// </summary>
        /// <param name="currentNeuron">The current neuron.</param>
        /// <param name="bestNeuron">The best neuron.</param>
        /// <returns>A percent that determines the amount of training the current
        /// neuron should get.Usually 100% when it is the bestNeuron.
        /// </returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            double[] vector = new double[_displacement.Length];
            int[] vectorCurrent = TranslateCoordinates(currentNeuron);
            int[] vectorBest = TranslateCoordinates(bestNeuron);
            for (int i = 0; i < vectorCurrent.Length; i++)
            {
                vector[i] = vectorCurrent[i] - vectorBest[i];
            }

            if (_hexagon)
            {
                double row = vector[1];
                double col = vector[0];
                double evenIndent = 1;
                double oddIndent = 2.5;
                double indent = ((row % 2 == 1) ? oddIndent : evenIndent);

                vector[1] = (int)(SQ75 + (row * SQ75));
                vector[0] = (int)(indent + (3 * col));

            }


            return _rbf.Evaluate(vector);

        }

        /// <summary>
        /// The radius.
        /// </summary>
        public double Radius
        {
            get { return _rbf.Width; }
            set { _rbf.Width = value; }
        }

        /// <summary>
        /// The RBF to use.
        /// </summary>
        public IFnRBF RBF
        {
            get { return _rbf; }
        }

        /// <summary>
        /// Translate the specified index into a set of multi-dimensional
        /// coordinates that represent the same index.This is how the
        /// multi-dimensional coordinates are translated into a one dimensional
        /// index for the input neurons.
        /// </summary>
        /// <param name="index">The index to translate.</param>
        /// <returns>The multi-dimensional coordinates.</returns>
        private int[] TranslateCoordinates(int index)
        {
            int[] result = new int[_displacement.Length];
            int countingIndex = index;

            for (int i = _displacement.Length - 1; i >= 0; i--)
            {
                int value;
                if (_displacement[i] > 0)
                {
                    value = countingIndex / _displacement[i];
                }
                else
                {
                    value = countingIndex;
                }

                countingIndex -= _displacement[i] * value;
                result[i] = value;

            }

            return result;
        }

        /// <summary>
        /// Sets or determines if this RBF is laid out as a hexigon.
        /// </summary>
        public bool Hexagon
        {
            get { return _hexagon; }
            set
            {
                if (value && _size.Length != 2)
                {
                    throw new AIFHError("Hexagon lattice can only be used in two dimensions.");
                }
                _hexagon = value;
            }
        }
    }
}
