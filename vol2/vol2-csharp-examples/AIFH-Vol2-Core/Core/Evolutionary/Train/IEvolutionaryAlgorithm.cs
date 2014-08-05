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
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Opp;
using AIFH_Vol2.Core.Evolutionary.Opp.Selection;
using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Score;
using AIFH_Vol2.Core.Evolutionary.Sort;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Learning.Score;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Train
{
    /// <summary>
    /// This interface defines the basic functionality of an Evolutionary Algorithm.
    /// An evolutionary algorithm is one that applies operations to a population of
    /// potential "solutions".
    /// </summary>
    public interface IEvolutionaryAlgorithm
    {
        /// <summary>
        /// Add an operation.
        /// </summary>
        /// <param name="probability">The probability of using this operator.</param>
        /// <param name="opp">The operator to add.</param>
        void AddOperation(double probability, IEvolutionaryOperator opp);

        /// <summary>
        /// Add a score adjuster. Score adjusters are used to adjust the adjusted
        /// score of a genome. This allows bonuses and penalties to be applied for
        /// desirable or undesirable traits. 
        /// </summary>
        /// <param name="scoreAdjust">The score adjustor to add.</param>
        void AddScoreAdjuster(IAdjustScore scoreAdjust);

        /// <summary>
        /// Calculate the score for a genome. 
        /// </summary>
        /// <param name="g">The genome to calculate the score for.</param>
        void CalculateScore(IGenome g);

        /// <summary>
        /// Called when training is finished. This allows the EA to properly shut
        /// down.
        /// </summary>
        void FinishTraining();

        /// <summary>
        /// Get the comparator that is used to choose the "true best" genome. This
        /// uses the real score, and not the adjusted score.
        /// </summary>
        IGenomeComparer BestComparer { get; set; }

        /// <summary>
        /// The current best genome. This genome is safe to use while the EA
        /// is running. Genomes are not modified. They simply produce
        /// "offspring".
        /// </summary>
        IGenome BestGenome { get; }

        /// <summary>
        /// The CODEC that is used to transform between genome and phenome.
        /// </summary>
        IGeneticCODEC CODEC { get; }

        /// <summary>
        /// The current score. This value should either be minimized or
        /// maximized, depending on the score function.
        /// </summary>
        double LastError { get; }

        /// <summary>
        /// The current iteration number. Also sometimes referred to as
        /// generation or epoch.
        /// </summary>
        int IterationNumber { get; }

        /// <summary>
        /// The maximum size an individual genome can be. This is an
        /// arbitrary number defined by the genome. Lower numbers are less
        /// complex.
        /// </summary>
        int MaxIndividualSize { get; }

        /// <summary>
        /// The maximum number to try certain genetic operations. This
        /// prevents endless loops.
        /// </summary>
        int MaxTries { get; }

        /// <summary>
        /// The operators.
        /// </summary>
        OperationList Operators { get; }

        /// <summary>
        /// The population.
        /// </summary>
        IPopulation Population { get; set;  }

        /// <summary>
        /// The score adjusters. This allows bonuses and penalties to be
        /// applied for desirable or undesirable traits.
        /// </summary>
        IList<IAdjustScore> ScoreAdjusters { get; }

        /// <summary>
        /// The score function.
        /// </summary>
        IScoreFunction ScoreFunction { get; }

        /// <summary>
        /// The selection operator. Used to choose genomes.
        /// </summary>
        ISelectionOperator Selection { get; set; }

        /// <summary>
        /// Get the comparator that is used to choose the "best" genome for
        /// selection, as opposed to the "true best". This uses the adjusted score,
        /// and not the score. 
        /// </summary>
        IGenomeComparer SelectionComparer { get; }

        /// <summary>
        /// True if exceptions that occur during genetic operations should be
        /// ignored.
        /// </summary>
        bool ShouldIgnoreExceptions { get; set; }

        /// <summary>
        /// The speciation method.
        /// </summary>
        ISpeciation Speciation { get; set; }

        /// <summary>
        /// True if any genome validators should be applied.
        /// </summary>
        bool ValidationMode { get; set; }

        /// <summary>
        /// Perform a training iteration. Also called generations or epochs.
        /// </summary>
        void Iteration();
    }
}
