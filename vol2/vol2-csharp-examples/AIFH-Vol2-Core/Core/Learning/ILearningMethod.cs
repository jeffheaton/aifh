using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning
{
    /// <summary>
    /// A learning method is used to train a machine learning algorithm to better classify or perform regression on
    /// input data.
    /// </summary>
    public interface ILearningMethod
    {
        /// <summary>
        /// Perform one training iteration.
        /// </summary>
        void Iteration();

        /// <summary>
        /// The error from the last training iteration.
        /// </summary>
        double LastError { get; }

        /// <summary>
        /// True, if we are done learning.  Not all learning algorithms know when they are done, in this case
        /// false is always returned.
        /// </summary>
        bool Done { get; }

        /// <summary>
        /// A string that indicates the status of training.
        /// </summary>
        String Status { get; }

        /// <summary>
        /// Should be called after the last iteration to make sure training completes any final tasks.
        /// </summary>
        void FinishTraining();
    }
}
