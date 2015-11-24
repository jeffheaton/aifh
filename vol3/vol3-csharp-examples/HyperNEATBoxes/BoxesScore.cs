using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Encog.Neural.Networks.Training;
using Encog.ML;

namespace HyperNEATBoxes
{
    /// <summary>
    ///  * The objective function for the boxes experiment. This score is described as
    /// follows in Kenneth O. Stanley's
    /// "A Hypercube-Based Indirect Encoding for Evolving Large-Scale Neural Networks"
    /// .
    /// 
    /// The field coordinates range between [-1, 1] in the x and y dimensions.
    /// However, the resolution within this range, i.e. the node density, can be
    /// varied. During evolution, the resolution of each field is fixed at 11 x 11.
    /// Thus the connective CPPN must learn to correctly connect a visual field of
    /// 121 inputs to a target field of 121 outputs, a total of 14,641 potential
    /// connection strengths.
    /// 
    /// During evolution, each individual in the population is evaluated for its
    /// ability to find the center of the bigger object. If the connectivity is not
    /// highly accurate, it is likely the substrate will often incorrectly choose the
    /// small object over the large one. Each individual evaluation thus includes 75
    /// trials, where each trial places the two objects at different locations. The
    /// trials are organized as follows. The small object appears at 25 uniformly
    /// distributed locations such that it is always completely within the visual
    /// field. For each of these 25 locations, the larger object is placed five units
    /// to the right, down, and diagonally, once per trial. The large object wraps
    /// around to the other side of the field when it hits the border. If the larger
    /// object is not completely within the visual field, it is moved the smallest
    /// distance possible that places it fully in view. Because of wrapping, this
    /// method of evaluation tests cases where the small object is on all possible
    /// sides of the large object. Thus many relative positions (though not all) are
    /// tested for a total number of 75 trials on the 11 by 11 substrate for each
    /// evaluation during evolution.
    /// 
    /// Within each trial, the substrate is activated over the entire visual field.
    /// The unit with the highest activation in the target field is interpreted as
    /// the substrate's selection. Fitness is calculated from the sum of the squared
    /// distances between the target and the point of highest activation over all 75
    /// trials. This fitness function rewards generalization and provides a smooth
    /// gradient for solutions that are close but not perfect.
    /// </summary>
    public class BoxesScore : ICalculateScore
    {
        public const double EDGE_LEN = 2.0;
        public const double SQR_LEN = 0.5772;
        public const double DIMENSIONS = 3;
        private int resolution;
        private double pixelSize;

        public BoxesScore(int theResolution)
        {
            resolution = theResolution;
            pixelSize = EDGE_LEN / theResolution;
        }

        public double CalculateScore(IMLMethod phenotype)
        {
            BoxTrialCase test = new BoxTrialCase(new Random());
            TrialEvaluation eval = new TrialEvaluation(phenotype, test);

            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < 25; j++)
                {
                    IntPair targetPos = eval.Test.InitTestCase(i);
                    IntPair actualPos = eval.Query(this.resolution);

                    eval.Accumulate(
                            CalcRealDistanceSquared(targetPos, actualPos),
                            Math.Max(0.0, eval.MaxActivation - eval.MinActivation));
                }
            }

            return eval.CalculateFitness();
        }

        private double CalcRealDistanceSquared(IntPair a, IntPair b)
        {
            double xdelta = (a.X - b.X) * pixelSize;
            double ydelta = (a.Y - b.Y) * pixelSize;
            return xdelta * xdelta + ydelta * ydelta;
        }

        public bool ShouldMinimize
        {
            get
            {
                return false;
            }
        }

        public bool RequireSingleThreaded
        {
            get
            {
                return false;
            }
        }
    }
}
