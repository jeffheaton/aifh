using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Train
{
    /// <summary>
    /// A class that owns a gradient calculation utility, usually a trainer.  This class provides the L1 and L2
    /// regularizaiton multipliers.
    /// </summary>
    public interface IGradientCalcOwner
    {
        /// <summary>
        /// How much to apply l1 regularization penalty, 0 (default) for none.
        /// </summary>
        double L1 { get; }

        /// <summary>
        /// How much to apply l2 regularization penalty, 0 (default) for none.
        /// </summary>
        double L2 { get; }
    }
}
