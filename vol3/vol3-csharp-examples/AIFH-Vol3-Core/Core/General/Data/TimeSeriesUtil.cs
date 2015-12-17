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
using AIFH_Vol3.Core.General.Data;

namespace AIFH_Vol3_Core.Core.General.Data
{
    /// <summary>
    ///     Utility for time series.
    /// </summary>
    public class TimeSeriesUtil
    {
        /// <summary>
        ///     Encode sliding window.
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

            var totalWindowSize = inputWindow + predictedWindow;

            var datasetIndex = 0;

            while (dataset.Length - datasetIndex >= totalWindowSize)
            {
                var item = new BasicData(
                    inputWindow*inputColumns.Length,
                    predictedWindow*predictedColumns.Length);

                // input columns
                var inputIdx = 0;
                for (var i = 0; i < inputWindow; i++)
                {
                    for (var j = 0; j < inputColumns.Length; j++)
                    {
                        item.Input[inputIdx++] = dataset[datasetIndex + i][inputColumns[j]];
                    }
                }
                // predicted columns
                var predictIdx = 0;
                for (var i = 0; i < predictedWindow; i++)
                {
                    for (var j = 0; j < predictedColumns.Length; j++)
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