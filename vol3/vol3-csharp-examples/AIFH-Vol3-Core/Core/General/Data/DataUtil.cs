using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.Util;
using CsvHelper;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.General.Data
{
    /// <summary>
    /// Several dataset utilities.
    /// </summary>
    public class DataUtil
    {
        /// <summary>
        /// Split a list into two sublists by randomly shuffling the values (without replacement).
        /// </summary>
        /// <typeparam name="T">The type that the lists contain.</typeparam>
        /// <param name="list">The list to split/shuffle.</param>
        /// <param name="ratio">The size of the first retuerned list.</param>
        /// <param name="rnd">A random number generator to split the lists.</param>
        /// <returns>A list containing the two split lists.</returns>
        public static IList<IList<T>> Split<T>(IList<T> list, double ratio, IGenerateRandom rnd)
        {
            IList<IList<T>> result = new List<IList<T>>();
            int aCount = (int)(list.Count * ratio);

            List<T> a = new List<T>();
            List<T> b = new List<T>();
            result.Add(a);
            result.Add(b);

            b.AddRange(list);

            for (int i = 0; i < aCount; i++)
            {
                int idx = rnd.NextInt(0, b.Count);
                a.Add(b[idx]);
                b.RemoveAt(idx);
            }

            return result;
        }

        /// <summary>
        /// Split a list into two sublists by randomly shuffling the values (without replacement).
        /// A new Mersenne twister random number generator will be used.
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
        /// Calculate error for regression. 
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
            foreach (BasicData item in dataset)
            {
                double[] output = model.ComputeRegression(item.Input);
                calc.UpdateError(output, item.Ideal, 1.0);
            }

            return calc.Calculate();
        }

        /// <summary>
        /// Calculate classification error. 
        /// </summary>
        /// <param name="data">The dataset.</param>
        /// <param name="model">The model to evaluate.</param>
        /// <returns>The error.</returns>
        public static double CalculateClassificationError(
                IList<BasicData> data,
                IClassificationAlgorithm model)
        {
            int total = 0;
            int correct = 0;

            foreach (BasicData pair in data)
            {
                int ideal = ArrayUtil.IndexOfLargest(pair.Ideal);
                int actual = model.ComputeClassification(pair.Input);
                if (actual == ideal)
                    correct++;
                total++;
            }
            return (double)(total - correct) / (double)total;

        }
        
        /// <summary>
        /// Dump a dataset as a CSV. 
        /// </summary>
        /// <param name="file">The file to dump to.</param>
        /// <param name="dataset">The dataset.</param>
        public static void DumpCSV(string file, IList<BasicData> dataset)
        {
            using (TextWriter tw = File.CreateText(file))
            {
                CsvWriter writer = new CsvWriter(tw);
                int inputCount = dataset[0].Input.Length;
                int outputCount = dataset[0].Ideal.Length;
                int totalCount = inputCount + outputCount;

                String[] headers = new String[totalCount];
                int idx = 0;
                for (int i = 0; i < inputCount; i++)
                {
                    headers[idx++] = "x" + i;
                }
                for (int i = 0; i < outputCount; i++)
                {
                    headers[idx++] = "y" + i;
                }
                writer.WriteRecord(headers);

                String[] line = new String[totalCount];
                for (int i = 0; i < dataset.Count; i++)
                {
                    BasicData item = dataset[i];

                    idx = 0;
                    for (int j = 0; j < inputCount; j++)
                    {
                        line[idx++] = item.Input[j].ToString(CultureInfo.InvariantCulture);
                    }
                    for (int j = 0; j < outputCount; j++)
                    {
                        line[idx++] = item.Ideal[j].ToString(CultureInfo.InvariantCulture);
                    }
                    writer.WriteRecord(line);
                }
            }
        }
    }
}
