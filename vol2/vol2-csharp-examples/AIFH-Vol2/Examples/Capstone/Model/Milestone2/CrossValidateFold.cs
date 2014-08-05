// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
