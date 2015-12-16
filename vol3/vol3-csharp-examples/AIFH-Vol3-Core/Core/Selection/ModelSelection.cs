using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    /// Base class for model selection searches.
    /// </summary>
    public abstract class ModelSelection
    {
        /// <summary>
        /// The axes.
        /// </summary>
        private IList<ISearchAxis> searchAxises = new List<ISearchAxis>();

        /// <summary>
        /// The search axes.
        /// </summary>
        public IList<ISearchAxis> SearchAxises
        {
            get
            {
                return this.searchAxises;
            }
        }
        
        /// <summary>
        /// Add a numeric axis. 
        /// </summary>
        /// <param name="start">The start of the axis.</param>
        /// <param name="stop">The end of the axis.</param>
        /// <param name="step">The step.</param>
        public void AddNumericAxis(double start, double stop, double step)
        {
            this.searchAxises.Add(new NumericSearchAxis(start, stop, step));
        }
        
        /// <summary>
        /// Add a category axis. 
        /// </summary>
        /// <param name="list">The list of categories.</param>
        public void AddCategoryAxis(String[] list)
        {
            this.searchAxises.Add(new CategorySearchAxis(list));
        }

        /// <summary>
        /// Get the next element to search. 
        /// </summary>
        /// <returns>The next element to search.</returns>
        public abstract Object[] Next();
    }
}
