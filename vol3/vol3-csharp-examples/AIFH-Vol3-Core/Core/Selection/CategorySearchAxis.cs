using AIFH_Vol3.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    /// An axis of categorical items.
    /// </summary>
    public class CategorySearchAxis : ISearchAxis
    {
        /// <summary>
        /// The list of categories.
        /// </summary>
        private IList<string> _categories = new List<string>();

        /// <summary>
        /// The current index.
        /// </summary>
        private int _currentIndex;

        /// <summary>
        /// Create a category search axis. 
        /// </summary>
        /// <param name="catList">The list of categories.</param>
        public CategorySearchAxis(String[] catList)
        {
            foreach (string str in catList)
            {
                _categories.Add(str);
            }
        }

        /// <summary>
        /// The categories.
        /// </summary>
        public IList<String> Categories
        {
            get
            {
                return _categories;
            }
        }

        /// <inheritdoc/>
        public void Reset()
        {
            _currentIndex = 0;
        }

        /// <inheritdoc/>
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

        /// <inheritdoc/>
        public Object CurrentState()
        {
            return _categories[_currentIndex];

        }

        /// <inheritdoc/>
        public Object Sample(IGenerateRandom rnd)
        {
            return _categories[rnd.NextInt(_categories.Count)];
        }
    }
}
