using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Distance;
using AIFH_Vol3.Core.Randomize;
using MathNet.Numerics.LinearAlgebra.Double;

namespace AIFH_Vol3_Core.Core.SOM
{

    public class SelfOrganizingMap
    {
        /// <summary>
        /// The weights.
        /// </summary>
        private Matrix _weights;

        /// <summary>
        /// Distance calculation.
        /// </summary>
        private ICalculateDistance calcDist = new EuclideanDistance();
        
        /// <summary>
        /// The constructor. 
        /// </summary>
        /// <param name="inputCount">Number of input neurons</param>
        /// <param name="outputCount">Number of output neurons</param>
        public SelfOrganizingMap(int inputCount, int outputCount)
        {
            _weights = DenseMatrix.Create(outputCount, inputCount, 0);
        }


        public double CalculateError(double[][] data)
        {
            BestMatchingUnit bmu = new BestMatchingUnit(this);

            bmu.Reset();

            // Determine the BMU for each training element.
            foreach (double[] pair in data)
            {
                double[] input = pair;
                bmu.CalculateBMU(input);
            }

            // update the error
            return bmu.WorstDistance / 100.0;
        }

        /// <inheritdoc/>
        public int Classify(double[] input)
        {
            if (input.Length > InputCount)
            {
                throw new AIFHError(
                        "Can't classify SOM with input size of " + InputCount
                                + " with input data of count " + input.Length);
            }

            double minDist = double.PositiveInfinity;
            int result = -1;

            var rowWeights = _weights.ToRowArrays();

            for (int i = 0; i < OutputCount; i++)
            {

                double dist = calcDist.Calculate(input, rowWeights[i]);
                if (dist < minDist)
                {
                    minDist = dist;
                    result = i;
                }
            }

            return result;
        }

        /// <inheritdoc/>
        public int InputCount
        {
            get { return _weights.ColumnCount; }
        }

        /// <inheritdoc/>
        public int OutputCount
        {
            get { return _weights.RowCount; }
        }

        /// <summary>
        /// The weights.
        /// </summary>
        public Matrix Weights
        {
            get { return _weights; }
        }


        public void Reset(IGenerateRandom rnd)
        {
            for (int i = 0; i < _weights.RowCount; i++)
            {
                for (int j = 0; j < _weights.ColumnCount; j++)
                {
                    _weights[i, j] = rnd.NextDouble(-1, 1);
                }
            }
        }

        public void Reset()
        {
            Reset(new MersenneTwisterGenerateRandom());
        }
        
        /// <summary>
        /// An alias for the classify method, kept for compatibility
        /// with earlier versions of Encog.
        /// </summary>
        /// <param name="input">The input pattern.</param>
        /// <returns>The winning neuron.</returns>
        public int Winner(double[] input)
        {
            return Classify(input);
        }
    }
}
