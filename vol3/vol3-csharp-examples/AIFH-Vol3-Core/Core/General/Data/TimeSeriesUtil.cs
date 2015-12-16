using AIFH_Vol3.Core.General.Data;
using System;
using System.Collections.Generic;

namespace AIFH_Vol3_Core.Core.General.Data
{
    /// <summary>
    /// Utility for time series.
    /// </summary>
    public class TimeSeriesUtil
    {
        /// <summary>
        /// Encode sliding window.
        /// </summary>
        /// <param name="dataset">The dataset.</param>
        /// <param name="inputWindow">The size of the input window.</param>
        /// <param name="predictedWindow">The size of the prediction window.</param>
        /// <param name="inputColumns">The number of input columns.</param>
        /// <param name="predictedColumns">The number of predicted columns.</param>
        /// <returns>The dataset.</returns>
        public static IList<BasicData> SlidingWindow(
                double[][] dataset,
                int inputWindow,
                int predictedWindow,
                int[] inputColumns,
                int[] predictedColumns)
        {

            IList<BasicData> result = new List<BasicData>();

            int totalWindowSize = inputWindow + predictedWindow;

            int datasetIndex = 0;

            while ((dataset.Length - datasetIndex) >= totalWindowSize)
            {
                BasicData item = new BasicData(
                        inputWindow * inputColumns.Length,
                        predictedWindow * predictedColumns.Length);

                // input columns
                int inputIdx = 0;
                for (int i = 0; i < inputWindow; i++)
                {
                    for (int j = 0; j < inputColumns.Length; j++)
                    {
                        item.Input[inputIdx++] = dataset[datasetIndex + i][inputColumns[j]];
                    }
                }
                // predicted columns
                int predictIdx = 0;
                for (int i = 0; i < predictedWindow; i++)
                {
                    for (int j = 0; j < predictedColumns.Length; j++)
                    {
                        item.Ideal[predictIdx++] = dataset[datasetIndex + inputWindow + i][predictedColumns[j]];
                    }
                }

                datasetIndex++;
                // add the data item
                result.Add(item);
            }

            return result;
        }
    }
}
