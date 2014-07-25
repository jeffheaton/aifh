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