// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
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

using System.Collections.Generic;
using System.Globalization;
using System.IO;
using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.Util;
using CsvHelper;

namespace AIFH_Vol3_Core.Core.General.Data
{
    /// <summary>
    ///     Several dataset utilities.
    /// </summary>
    public class DataUtil
    {
        /// <summary>
        ///     Split a list into two sublists by randomly shuffling the values (without replacement).
        /// </summary>
        /// <typeparam name="T">The type that the lists contain.</typeparam>
        /// <param name="list">The list to split/shuffle.</param>
        /// <param name="ratio">The size of the first retuerned list.</param>
        /// <param name="rnd">A random number generator to split the lists.</param>
        /// <returns>A list containing the two split lists.</returns>
        public static IList<IList<T>> Split<T>(IList<T> list, double ratio, IGenerateRandom rnd)
        {
            IList<IList<T>> result = new List<IList<T>>();
            var aCount = (int) (list.Count*ratio);

            var a = new List<T>();
            var b = new List<T>();
            result.Add(a);
            result.Add(b);

            b.AddRange(list);

            for (var i = 0; i < aCount; i++)
            {
                var idx = rnd.NextInt(0, b.Count);
                a.Add(b[idx]);
                b.RemoveAt(idx);
            }

            return result;
        }

        /// <summary>
        ///     Split a list into two sublists by randomly shuffling the values (without replacement).
        ///     A new Mersenne twister random number generator will be used.
        /// </summary>
        /// <typeparam name="T">The type that the lists contain.</typeparam>
        /// <param name="list">The list to split/shuffle.</param>
        /// <param name="ratio">The size of the first retuerned list.</param>
        /// <returns>A list containing the two split lists.</returns>
        public static IList<IList<T>> Split<T>(IList<T> list, double ratio)
        {
            return Split(list, ratio, new MersenneTwisterGenerateRandom());
        }

        /// <summary>
        ///     Calculate error for regression.
        /// </summary>
        /// <param name="dataset">The dataset.</param>
        /// <param name="model">The model to evaluate.</param>
        /// <param name="calc">The error calculation.</param>
        /// <returns>The error.</returns>
        public static double CalculateRegressionError(IList<BasicData> dataset,
            IRegressionAlgorithm model,
            IErrorCalculation calc)
        {
            calc.Clear();
            foreach (var item in dataset)
            {
                var output = model.ComputeRegression(item.Input);
                calc.UpdateError(output, item.Ideal, 1.0);
            }

            return calc.Calculate();
        }

        /// <summary>
        ///     Calculate classification error.
        /// </summary>
        /// <param name="data">The dataset.</param>
        /// <param name="model">The model to evaluate.</param>
        /// <returns>The error.</returns>
        public static double CalculateClassificationError(
            IList<BasicData> data,
            IClassificationAlgorithm model)
        {
            var total = 0;
            var correct = 0;

            foreach (var pair in data)
            {
                var ideal = ArrayUtil.IndexOfLargest(pair.Ideal);
                var actual = model.ComputeClassification(pair.Input);
                if (actual == ideal)
                    correct++;
                total++;
            }
            return (total - correct)/(double) total;
        }

        /// <summary>
        ///     Dump a dataset as a CSV.
        /// </summary>
        /// <param name="file">The file to dump to.</param>
        /// <param name="dataset">The dataset.</param>
        public static void DumpCSV(string file, IList<BasicData> dataset)
        {
            using (TextWriter tw = File.CreateText(file))
            {
                var writer = new CsvWriter(tw);
                var inputCount = dataset[0].Input.Length;
                var outputCount = dataset[0].Ideal.Length;
                var totalCount = inputCount + outputCount;

                var headers = new string[totalCount];
                var idx = 0;
                for (var i = 0; i < inputCount; i++)
                {
                    headers[idx++] = "x" + i;
                }
                for (var i = 0; i < outputCount; i++)
                {
                    headers[idx++] = "y" + i;
                }
                writer.WriteRecord(headers);

                var line = new string[totalCount];
                for (var i = 0; i < dataset.Count; i++)
                {
                    var item = dataset[i];

                    idx = 0;
                    for (var j = 0; j < inputCount; j++)
                    {
                        line[idx++] = item.Input[j].ToString(CultureInfo.InvariantCulture);
                    }
                    for (var j = 0; j < outputCount; j++)
                    {
                        line[idx++] = item.Ideal[j].ToString(CultureInfo.InvariantCulture);
                    }
                    writer.WriteRecord(line);
                }
            }
        }
    }
}