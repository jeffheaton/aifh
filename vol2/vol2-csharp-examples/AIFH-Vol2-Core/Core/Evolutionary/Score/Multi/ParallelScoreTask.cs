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
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
//
// Encog(tm) Core v3.2 - .Net Version
// http://www.heatonresearch.com/encog/
//
// Copyright 2008-2014 Heaton Research, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//  http://www.apache.org/licenses/LICENSE-2.0
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
using System;
using System.Collections.Generic;

namespace AIFH_Vol2.Core.Evolutionary.Score.Multi
{
    public class ParallelScoreTask
    {
        /// <summary>
        ///     The score adjusters.
        /// </summary>
        private readonly IList<IAdjustScore> adjusters;

        /// <summary>
        ///     The genome to calculate the score for.
        /// </summary>
        private readonly IGenome genome;

        /// <summary>
        ///     The owners.
        /// </summary>
        private readonly ParallelScore owner;

        /// <summary>
        ///     The score function.
        /// </summary>
        private readonly IScoreFunction scoreFunction;

        /// <summary>
        ///     Construct the parallel task.
        /// </summary>
        /// <param name="genome">The genome.</param>
        /// <param name="theOwner">The owner.</param>
        public ParallelScoreTask(IGenome genome, ParallelScore theOwner)
        {
            owner = theOwner;
            this.genome = genome;
            scoreFunction = theOwner.ScoreFunction;
            adjusters = theOwner.Adjusters;
        }

        /// <summary>
        ///     Perform the task.
        /// </summary>
        public void PerformTask()
        {
            IMLMethod phenotype = owner.CODEC.Decode(genome);
            if (phenotype != null)
            {
                double score;
                try
                {
                    score = scoreFunction.CalculateScore(phenotype);
                }
                catch (AIFHError e)
                {
                    score = Double.NaN;
                }
                genome.Score = score;
                genome.AdjustedScore = score;
                BasicEA.CalculateScoreAdjustment(genome, adjusters);
            }
        }
    }
}
