// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//
using System;
using System.Collections.Generic;
using AIFH_Vol2.Core.General;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Normalize;

namespace AIFH_Vol2.Examples.Util
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
