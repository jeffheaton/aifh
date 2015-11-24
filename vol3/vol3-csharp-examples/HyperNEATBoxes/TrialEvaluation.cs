using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Encog.ML;
using Encog.ML.Data;
using Encog.ML.Data.Basic;
using Encog.Neural.NEAT;

namespace HyperNEATBoxes
{
    public class TrialEvaluation
    {
        private IMLMethod phenotype;
        private BoxTrialCase test;
        public double AccDistance { get; set; }
        public double AccRange { get; set; }
        private double minActivation;
        private double maxActivation;
        private IMLData output;

        public TrialEvaluation(IMLMethod thePhenotype, BoxTrialCase theTest)
        {
            this.phenotype = thePhenotype;
            this.test = theTest;
        }

        public IMLMethod Phenotype
        {
            get
            {
                return phenotype;
            }
        }

        public BoxTrialCase Test
        {
            get
            {
                return test;
            }
        }

        public void Accumulate(double distance, double range)
        {
            this.AccDistance += distance;
            this.AccRange += range;
        }

        public double CalculateFitness()
        {
            double threshold = BoxesScore.EDGE_LEN * BoxesScore.SQR_LEN;
            double rmsd = Math.Sqrt(this.AccDistance / 75.0);
            double fitness;
            if (rmsd > threshold)
            {
                fitness = 0.0;
            }
            else
            {
                fitness = (((threshold - rmsd) * 100.0) / threshold) + (this.AccRange / 7.5);
            }

            return fitness;
        }

        public IntPair Query(int resolution)
        {
            // first, create the input data
            int index = 0;
            BasicMLData inputData = new BasicMLData(resolution * resolution);
            double pixelSize = 2.0 / resolution;
            double orig = -1.0 + (pixelSize / 2.0);

            double yReal = orig;
            for (int y = 0; y < resolution; y++, yReal += pixelSize)
            {
                double xReal = orig;
                for (int x = 0; x < resolution; x++, xReal += pixelSize)
                {
                    inputData.Data[index] = this.test.GetPixel(xReal, yReal);
                    index++;
                }
            }

            // second, query the network
            output = ((NEATNetwork)this.phenotype).Compute(inputData);

            // finally, process the output
            minActivation = Double.PositiveInfinity;
            maxActivation = Double.NegativeInfinity;
            int maxIndex = 0;

            for (int i = 0; i < output.Count; i++)
            {
                double d = output[i];

                if (d > maxActivation)
                {
                    maxActivation = d;
                    maxIndex = i;
                }
                else if (d < minActivation)
                {
                    minActivation = d;
                }
            }

            int yy = maxIndex / resolution;
            int xx = maxIndex - (yy * resolution);
            return new IntPair(xx, yy);
        }

        public double MinActivation
        {
            get
            {
                return minActivation;
            }
        }

        public double MaxActivation
        {
            get
            {
                return maxActivation;
            }
        }

        public IMLData Output
        {
            get
            {
                return output;
            }
        }

        public int Normalize(double d, int i)
        {
            int result = (int)(((d - this.minActivation) / (this.maxActivation - this.minActivation)) * i);
            if (result < 0)
            {
                result = 0;
            }
            if (result > 255)
            {
                result = 255;
            }
            return result;
        }

    }
}
