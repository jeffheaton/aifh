
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Core.Learning
{
    /// <summary>
    /// Defines a MLMethod that can hold context.  This allows the context to be
    /// cleared.  Examples of MLMethod objects that support this are NEAT,
    /// Elmann and Jordan.
    /// </summary>
    public interface IMLContext
    {
        /// <summary>
        /// Clear the context.
        /// </summary>
        void ClearContext();
    }
}
