using System;
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Genetic.Genome;

namespace AIFH_Vol2.Core.Genetic.Species
{
    public class ArraySpeciation<T> : ThresholdSpeciation
    {
        public override double GetCompatibilityScore(IGenome genome1, IGenome genome2)
        {
            if (genome1 is DoubleArrayGenome)
            {
                return ScoreDouble(genome1, genome2);
            }
            if (genome1 is IntegerArrayGenome)
            {
                return ScoreInt(genome1, genome2);
            }
            throw new AIFHError("This speciation does not support: " + genome1.GetType().Name);
        }

        private double ScoreInt(IGenome genome1, IGenome genome2)
        {
            var intGenome1 = (IntegerArrayGenome) genome1;
            var intGenome2 = (IntegerArrayGenome) genome2;
            double sum = 0;
            for (int i = 0; i < intGenome1.Count; i++)
            {
                double diff = intGenome1.Data[i] - intGenome2.Data[i];
                sum += diff*diff;
            }
            return Math.Sqrt(sum);
        }

        private double ScoreDouble(IGenome genome1, IGenome genome2)
        {
            var doubleGenome1 = (DoubleArrayGenome) genome1;
            var doubleGenome2 = (DoubleArrayGenome) genome2;
            double sum = 0;
            for (int i = 0; i < doubleGenome1.Count; i++)
            {
                double diff = doubleGenome1.Data[i] - doubleGenome2.Data[i];
                sum += diff*diff;
            }
            return Math.Sqrt(sum);
        }
    }
}