using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    /// Perform a grid search over the axes.
    /// </summary>
    public class GridModelSelection : ModelSelection
    {
        /// <summary>
        /// Is the search done?
        /// </summary>
        private bool _done;

        /// <inheritdoc/>
        public override Object[] Next()
        {
            if (_done)
            {
                return null;
            }

            Object[] result = new Object[SearchAxises.Count];

            for (int i = 0; i < result.Length; i++)
            {
                result[i] = SearchAxises[i].CurrentState();
            }

            int idx = 0;
            while (SearchAxises[idx].Advance())
            {
                idx++;
                if (idx >= SearchAxises.Count)
                {
                    _done = true;
                    break;
                }
            }
            return result;
        }
    }
}
