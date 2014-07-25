using System.Collections.Generic;
using AIFH_Vol2.Core.General.Data;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone2
{
    /// <summary>
    /// A cross validation fold.  This contains a training and validation set.  A score is also
    /// held for the validation sets.
    /// </summary>
    public class CrossValidateFold
    {
        /// <summary>
        /// The training set.
        /// </summary>
        private readonly IList<BasicData> _trainingSet = new List<BasicData>();

        /// <summary>
        /// The validation set.
        /// </summary>
        private readonly IList<BasicData> _validationSet = new List<BasicData>();

        /// <summary>
        /// The score.
        /// </summary>
        public double Score { get; set; }

        /// <summary>
        /// The training set.
        /// </summary>
        public IList<BasicData> TrainingSet
        {
            get { return _trainingSet; }
        }

        /// <summary>
        /// The validation set.
        /// </summary>
        public IList<BasicData> ValidationSet
        {
            get { return _validationSet; }
        }

    }
}
