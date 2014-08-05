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
using AIFH_Vol2.Core.Evolutionary.Opp;
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Randomize;
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

namespace AIFH_Vol2.Core.Evolutionary.Train
{
    /// <summary>
    ///     A worker thread for an Evolutionary Algorithm.
    /// </summary>
    public class EAWorker
    {
        /// <summary>
        ///     The children genomes.
        /// </summary>
        private readonly IGenome[] _children;

        /// <summary>
        ///     The parent genomes.
        /// </summary>
        private readonly IGenome[] _parents;

        /// <summary>
        ///     Random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd;

        /// <summary>
        ///     The species being processed.
        /// </summary>
        private readonly ISpecies _species;

        /// <summary>
        ///     The parent object.
        /// </summary>
        private readonly BasicEA _train;

        /// <summary>
        ///     Construct the EA worker.
        /// </summary>
        /// <param name="theTrain">The trainer.</param>
        /// <param name="theSpecies">The species.</param>
        public EAWorker(BasicEA theTrain, ISpecies theSpecies)
        {
            _train = theTrain;
            _species = theSpecies;
            _rnd = _train.RandomNumberFactory.Factor();

            _parents = new IGenome[_train.Operators.MaxParents];
            _children = new IGenome[_train.Operators.MaxOffspring];
        }

        /// <summary>
        ///     Choose a parent.
        /// </summary>
        /// <returns>The chosen parent.</returns>
        private IGenome ChooseParent()
        {
            int idx = _train.Selection.PerformSelection(_rnd,
                                                       _species);
            return _species.Members[idx];
        }

        /// <summary>
        ///     Perform one operation.
        /// </summary>
        public void PerformTask()
        {
            bool success = false;
            int tries = _train.MaxOperationErrors;
            do
            {
                try
                {
                    // choose an evolutionary operation (i.e. crossover or a type of
                    // mutation) to use
                    IEvolutionaryOperator opp = _train.Operators
                                                     .PickMaxParents(_rnd,
                                                                     _species.Members.Count);

                    _children[0] = null;

                    // prepare for either sexual or asexual reproduction either way,
                    // we need at least one parent, which is the first parent.
                    //
                    // Chose the first parent, there must be at least one genome in
                    // this species
                    _parents[0] = ChooseParent();

                    // if the number of individuals in this species is only
                    // one then we can only clone and perhaps mutate, otherwise use
                    // the crossover probability to determine if we are to use
                    // sexual reproduction.
                    if (opp.ParentsNeeded > 1)
                    {
                        int numAttempts = 5;

                        _parents[1] = ChooseParent();
                        while (_parents[0] == _parents[1]
                               && numAttempts-- > 0)
                        {
                            _parents[1] = ChooseParent();
                        }

                        // success, perform crossover
                        if (_parents[0] != _parents[1])
                        {
                            opp.PerformOperation(_rnd, _parents, 0,
                                                 _children, 0);
                        }
                    }
                    else
                    {
                        // clone a child (asexual reproduction)
                        opp.PerformOperation(_rnd, _parents, 0,
                                             _children, 0);
                        _children[0].Population = _parents[0].Population;
                    }

                    // process the new child
                    foreach (IGenome child in _children)
                    {
                        if (child != null)
                        {
                            child.Population = _parents[0].Population;

                            child.BirthGeneration = _train.IterationNumber;

                            _train.CalculateScore(child);
                            if (!_train.AddChild(child))
                            {
                                return;
                            }
                            success = true;

                        }
                    }
                }
                catch (AIFHError)
                {
                    tries--;
                    if (tries < 0)
                    {
                        throw new AIFHError(
                            "Could not perform a successful genetic operaton after "
                            + _train.MaxOperationErrors
                            + " tries.");
                    }
                }
                catch (Exception t)
                {
                    if (!_train.IgnoreExceptions)
                    {
                        throw;
                    }
                }
            } while (!success);
        }
    }
}
