
using System;

namespace AIFH_Vol3.Core
{
    /// <summary>
    /// General exception class.
    /// </summary>
    public class AIFHError : Exception
    {
        /// <summary>
        /// Wraps an exception.
        /// </summary>
        /// <param name="msg">The message.</param>
        /// <param name="t">The exception.</param>
        public AIFHError(string msg, Exception t)
            : base(msg, t)
        {
        }

        /// <summary>
        /// A string exception.
        /// </summary>
        /// <param name="s">The message.</param>
        public AIFHError(string s)
            : base(s)
        {
        }
    }
}
