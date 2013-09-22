using System;
using System.Collections.Generic;
using AIFH_Vol1.Core.General;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Learning;
using AIFH_Vol1.Core.Normalize;

namespace AIFH_Vol1.Examples.Learning
{
    /// <summary>
    /// Base class for many of the iteration based examples.  It will loop over iterations and 
    /// display stats.
    /// </summary>
    public class SimpleLearn
    {
        /// <summary>
        /// Perform training iterations.
        /// </summary>
        /// <param name="train">The learning algorithm.</param>
        /// <param name="maxIterations">The max number of iterations.</param>
        /// <param name="targetScore">The target score.</param>
        /// <param name="shouldMinimize">True, if we should minimize.</param>
        public void PerformIterations(ILearningMethod train, int maxIterations, double targetScore, bool shouldMinimize)
        {
            int iterationNumber = 0;
            bool done = false;

            do
            {
                iterationNumber++;

                train.Iteration();

                if (train.Done)
                {
                    done = true;
                }
                else if (iterationNumber >= maxIterations)
                {
                    done = true;
                }
                else if (shouldMinimize && train.LastError < targetScore)
                {
                    done = true;
                }
                else if (!shouldMinimize && train.LastError > targetScore)
                {
                    done = true;
                }

                Console.WriteLine("Iteration #" + iterationNumber + ", Score=" + train.LastError + ", " + train.Status);
            } while (!done);

            train.FinishTraining();
            Console.WriteLine("Final score: " + train.LastError);
        }

        /// <summary>
        /// Query a regression algorithm and see how close it matches the training data.
        /// </summary>
        /// <param name="alg">The algorithm to evaluate.</param>
        /// <param name="theTrainingData">The training data.</param>
        public static void Query(IRegressionAlgorithm alg, IList<BasicData> theTrainingData)
        {
            foreach (BasicData data in theTrainingData)
            {
                double[] output = alg.ComputeRegression(data.Input);
                Console.WriteLine(VectorUtil.DoubleArrayToString(data.Input) + " -> " + VectorUtil.DoubleArrayToString(output) + ", Ideal: " + VectorUtil.DoubleArrayToString(data.Ideal));
            }
        }

        /// <summary>
        /// Query a regression algorithm using equilateral encoding.
        /// </summary>
        /// <param name="alg">The algorithm being used.</param>
        /// <param name="theTrainingData">The training data.</param>
        /// <param name="items">The category items classified.</param>
        /// <param name="high">The high value.</param>
        /// <param name="low">The low value.</param>
        public static void QueryEquilateral(
                IRegressionAlgorithm alg,
                IList<BasicData> theTrainingData,
                IDictionary<String, int> items,
                double high, double low)
        {
            // first, we need to invert the items.  Right now it maps from category to index.  We need index to category.
            IDictionary<int, String> invMap = new Dictionary<int, string>();
            foreach (string key in items.Keys)
            {
                int value = items[key];
                invMap[value] = key;
            }

            // now we can query
            Equilateral eq = new Equilateral(items.Count, high, low);
            foreach (BasicData data in theTrainingData)
            {
                double[] output = alg.ComputeRegression(data.Input);
                int idealIndex = eq.Decode(data.Ideal);
                int actualIndex = eq.Decode(output);
                Console.WriteLine(VectorUtil.DoubleArrayToString(data.Input) + " -> " + invMap[actualIndex]
                        + ", Ideal: " + invMap[idealIndex]);
            }
        }

        /// <summary>
        /// Query a regression algorithm using one-of-n encoding.
        /// </summary>
        /// <param name="alg">The algorithm being used.</param>
        /// <param name="theTrainingData">The training data.</param>
        /// <param name="items">The category items classified.</param>
        public static void QueryOneOfN(
                IRegressionAlgorithm alg,
                IList<BasicData> theTrainingData,
                IDictionary<String, int> items)
        {
            // first, we need to invert the items.  Right now it maps from category to index.  We need index to category.
            IDictionary<int, String> invMap = new Dictionary<int, string>();
            foreach (string key in items.Keys)
            {
                int value = items[key];
                invMap[value] = key;
            }

            // now we can query
            foreach (BasicData data in theTrainingData)
            {
                double[] output = alg.ComputeRegression(data.Input);
                int idealIndex = VectorUtil.MaxIndex(data.Ideal);
                int actualIndex = VectorUtil.MaxIndex(output);
                Console.WriteLine(VectorUtil.DoubleArrayToString(data.Input) + " -> "
                    + invMap[actualIndex]
                        + ", Ideal: " + invMap[idealIndex]);
            }
        }
    }
}
