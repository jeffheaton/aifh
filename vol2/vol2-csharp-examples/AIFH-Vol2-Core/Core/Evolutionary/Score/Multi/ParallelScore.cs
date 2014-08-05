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
using AIFH_Vol2.Core.Evolutionary.CODEC;
using AIFH_Vol2.Core.Evolutionary.Population;
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
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Score.Multi
{
    /// <summary>
    ///     This class is used to calculate the scores for an entire population. This is
    ///     typically done when a new population must be scored for the first time.
    /// </summary>
    public class ParallelScore
    {
        /// <summary>
        ///     The score adjuster.
        /// </summary>
        private readonly IList<IAdjustScore> _adjusters;

        /// <summary>
        ///     The CODEC used to create genomes.
        /// </summary>
        private readonly IGeneticCODEC _codec;

        /// <summary>
        ///     The population to score.
        /// </summary>
        private readonly IPopulation _population;

        /// <summary>
        ///     The scoring function.
        /// </summary>
        private readonly IScoreFunction _scoreFunction;

        /// <summary>
        ///     Construct the parallel score calculation object.
        /// </summary>
        /// <param name="thePopulation">The population to score.</param>
        /// <param name="theCODEC">The CODEC to use.</param>
        /// <param name="theAdjusters">The score adjusters to use.</param>
        /// <param name="theScoreFunction">The score function.</param>
        /// <param name="theThreadCount">The requested thread count.</param>
        public ParallelScore(IPopulation thePopulation, IGeneticCODEC theCODEC,
                             IList<IAdjustScore> theAdjusters, IScoreFunction theScoreFunction,
                             int theThreadCount)
        {
            _codec = theCODEC;
            _population = thePopulation;
            _scoreFunction = theScoreFunction;
            _adjusters = theAdjusters;
            ThreadCount = theThreadCount;
        }

        /// <summary>
        ///     The thread count.
        /// </summary>
        public int ThreadCount { get; set; }

        /// <summary>
        ///     The population.
        /// </summary>
        public IPopulation Population
        {
            get { return _population; }
        }

        /// <summary>
        ///     The score function.
        /// </summary>
        public IScoreFunction ScoreFunction
        {
            get { return _scoreFunction; }
        }

        /// <summary>
        ///     The CODEC.
        /// </summary>
        public IGeneticCODEC CODEC
        {
            get { return _codec; }
        }

        /// <summary>
        ///     The score adjusters.
        /// </summary>
        public IList<IAdjustScore> Adjusters
        {
            get { return _adjusters; }
        }

        /// <summary>
        ///     Calculate the scores.
        /// </summary>
        public void Process()
        {
            // calculate workload
            IList<ParallelScoreTask> tasks = (from species in _population.Species from genome in species.Members select new ParallelScoreTask(genome, this)).ToList();

            // determine thread usage
            if (ThreadCount == 1)
            {
                // single
                foreach (ParallelScoreTask task in tasks)
                {
                    task.PerformTask();
                }
            }
            else
            {
                // parallel
                Parallel.ForEach(tasks, currentTask => currentTask.PerformTask());
            }
        }
    }
}
