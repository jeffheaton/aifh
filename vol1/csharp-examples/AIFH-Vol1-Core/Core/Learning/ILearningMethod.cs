// Artificial Intelligence for Humans
// Volume 1: Fundamental Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2013 by Jeff Heaton
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
namespace AIFH_Vol1.Core.Learning
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
        string Status
        {
            get;
        }

        /// <summary>
        /// Should be called after the last iteration to make sure training completes any final tasks.
        /// </summary>
        void FinishTraining();
    }
}
