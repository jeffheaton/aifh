using System.Collections.Generic;
using System.Linq;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone2
{
    /// <summary>
    ///     Used to perform a k-leave out cross validation.
    ///     This works by breaking the data set into k (often 5) random subsets.  From this we can create 5 training &
    ///     validation
    ///     sets. Each validation set becomes one of the k random subsets.  For each validation set a corresponding training
    ///     set
    ///     is created by using all data, but leaving out the validation set.
    ///     http://en.wikipedia.org/wiki/Cross-validation_(statistics)
    /// </summary>
    public class CrossValidate
    {
        /// <summary>
        ///     The folds of the cross validation.
        /// </summary>
        private readonly IList<CrossValidateFold> _folds = new List<CrossValidateFold>();

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="k">The number of folds.</param>
        /// <param name="training">The training set.</param>
        /// <param name="rnd">A random number generator.</param>
        public CrossValidate(int k, IEnumerable<BasicData> training, IGenerateRandom rnd)
        {
            IList<BasicData> temp = new List<BasicData>();
            temp = temp.Union(training).ToList();

            // Setup k validation sets.
            for (int i = 0; i < k; i++)
            {
                _folds.Add(new CrossValidateFold());
            }

            // Divide over the k sets.
            int leaveOutSet = 0;

            while (temp.Count > 0)
            {
                int idx = rnd.NextInt(temp.Count);
                BasicData item = temp[idx];
                temp.RemoveAt(idx);

                _folds[leaveOutSet].ValidationSet.Add(item);
                for (int includeSet = 0; includeSet < _folds.Count; includeSet++)
                {
                    if (includeSet != leaveOutSet)
                    {
                        _folds[includeSet].TrainingSet.Add(item);
                    }
                }

                leaveOutSet++;
                if (leaveOutSet >= k)
                {
                    leaveOutSet = 0;
                }
            }
        }

        /// <summary>
        ///     The folds.
        /// </summary>
        public IList<CrossValidateFold> Folds
        {
            get { return _folds; }
        }

        /// <summary>
        ///     The average score over all folds.
        /// </summary>
        public double Score
        {
            get
            {
                double sum = _folds.Sum(fold => fold.Score);
                return sum/_folds.Count;
            }
        }

        /// <summary>
        ///     The number of folds.
        /// </summary>
        public int Count
        {
            get { return _folds.Count; }
        }
    }
}