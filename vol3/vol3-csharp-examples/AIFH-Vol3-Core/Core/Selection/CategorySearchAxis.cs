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
using AIFH_Vol3.Core.Randomize;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    ///     An axis of categorical items.
    /// </summary>
    public class CategorySearchAxis : ISearchAxis
    {
        /// <summary>
        ///     The list of categories.
        /// </summary>
        private readonly IList<string> _categories = new List<string>();

        /// <summary>
        ///     The current index.
        /// </summary>
        private int _currentIndex;

        /// <summary>
        ///     Create a category search axis.
        /// </summary>
        /// <param name="catList">The list of categories.</param>
        public CategorySearchAxis(string[] catList)
        {
            foreach (var str in catList)
            {
                _categories.Add(str);
            }
        }

        /// <summary>
        ///     The categories.
        /// </summary>
        public IList<string> Categories
        {
            get { return _categories; }
        }

        /// <inheritdoc />
        public void Reset()
        {
            _currentIndex = 0;
        }

        /// <inheritdoc />
        public bool Advance()
        {
            _currentIndex++;
            if (_currentIndex >= _categories.Count)
            {
                _currentIndex = 0;
                return true;
            }
            return false;
        }

        /// <inheritdoc />
        public object CurrentState()
        {
            return _categories[_currentIndex];
        }

        /// <inheritdoc />
        public object Sample(IGenerateRandom rnd)
        {
            return _categories[rnd.NextInt(_categories.Count)];
        }
    }
}