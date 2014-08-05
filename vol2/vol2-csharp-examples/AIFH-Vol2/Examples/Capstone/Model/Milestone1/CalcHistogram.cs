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

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone1
{
    /// <summary>
    ///     Calculate a histogram of string values.  A count is kept for each unique value.
    /// </summary>
    public class CalcHistogram
    {
        /// <summary>
        ///     The counters for each unique value.
        /// </summary>
        private readonly IDictionary<string, int> histogram = new Dictionary<string, int>();

        /// <summary>
        ///     The histogram map.
        /// </summary>
        public IDictionary<string, int> Histogram
        {
            get { return histogram; }
        }

        public void Update(String key)
        {
            // See if we already have an entry
            if (histogram.ContainsKey(key))
            {
                int count = histogram[key];
                histogram[key] = count + 1;
            }
            else
            {
                // no entry, so create one at 1.
                histogram[key] = 1;
            }
        }

        /// <summary>
        ///     The string key that has the greatest frequency.
        /// </summary>
        /// <returns>The string key that has the greatest frequency.</returns>
        public String Max()
        {
            int maxCount = 0;
            String result = null;

            foreach (String key in histogram.Keys)
            {
                int count = histogram[key];
                if ((result == null) || (maxCount < count) || (maxCount == count && result.CompareTo(key) < 0))
                {
                    result = key;
                    maxCount = count;
                }
            }
            return result;
        }

        /// <summary>
        ///     The string key with the lowest frequency.
        /// </summary>
        /// <returns>The string key with the lowest frequency.</returns>
        public String Min()
        {
            int maxCount = 0;
            String result = null;

            foreach (string key in histogram.Keys)
            {
                int count = histogram[key];
                if ((result == null) || (maxCount > count) || (maxCount == count && result.CompareTo(key) > 0))
                {
                    result = key;
                    maxCount = count;
                }
            }
            return result;
        }
    }
}
