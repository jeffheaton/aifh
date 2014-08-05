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
using System;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2_MergePhysics.Physics;

namespace AIFH_Vol2_MergePhysics.Universe
{
    /// <summary>
    ///     Used to run a universe's physics and provide display.
    /// </summary>
    public class UniverseRunner
    {
        /// <summary>
        ///     The physics calculator.
        /// </summary>
        private readonly IPhysics _physics;

        /// <summary>
        ///     The universe that is used to generate the next frame.
        /// </summary>
        private readonly UniverseHolder _tempUniverse;

        /// <summary>
        ///     The universe.
        /// </summary>
        private readonly UniverseHolder _universe;

        /// <summary>
        ///     The difference between two frames.
        /// </summary>
        private double _diff;

        /// <summary>
        ///     The current iteration.
        /// </summary>
        private int _iteration;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="theUniverse">The universe.</param>
        /// <param name="thePhysics">The physics calculator.</param>
        public UniverseRunner(UniverseHolder theUniverse, IPhysics thePhysics)
        {
            _universe = theUniverse;
            _tempUniverse = (UniverseHolder) theUniverse.Clone();
            _physics = thePhysics;
        }

        /// <summary>
        ///     Should we automatically kill a universe that stabilizes.
        /// </summary>
        public bool AutoKill { get; set; }

        /// <summary>
        ///     The difference between the last two frames.
        /// </summary>
        public double Diff
        {
            get { return _diff; }
        }

        /// <summary>
        ///     The total number of iterations.
        /// </summary>
        public int Iterations
        {
            get { return _iteration; }
        }

        /// <summary>
        ///     The physics calculator.
        /// </summary>
        public IPhysics PhysicsRules
        {
            get { return _physics; }
        }

        /// <summary>
        ///     Advance one frame.
        /// </summary>
        /// <param name="rnd">The random number generator.</param>
        public void Advance(IGenerateRandom rnd)
        {
            int height = _universe.Height;
            int width = _universe.Width;


            // Copy the current universe to the temp universe.
            // The next frame is rendered into the temp universe.
            lock (this)
            {
                _tempUniverse.Copy(_universe);

                // Run each pixel through the physics calculator.
                for (int col = 0; col < width; col++)
                {
                    for (int row = 0; row < height; row++)
                    {
                        _physics.ProcessPixel(_tempUniverse, row, col);
                    }
                }

                _diff = _tempUniverse.Compare(_universe);

                _iteration++;

                // Copy the temp universe back into
                _universe.Copy(_tempUniverse);

                if (_diff < 0.0001 && _iteration > 5)
                {
                    if (AutoKill)
                    {
                        Reset(rnd);
                    }
                }
            }
        }

        /// <summary>
        ///     Perform a genetic crossover between two parent universes.  A new universe will be created with attributes
        ///     from the two parents.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <param name="crossoverParent1">The first parent.</param>
        /// <param name="crossoverParent2">The second parent.</param>
        public
            void Crossover(IGenerateRandom rnd, UniverseRunner crossoverParent1,
                UniverseRunner crossoverParent2)
        {
            double[] parent1 = crossoverParent1.PhysicsRules.Data;
            double[] parent2 = crossoverParent2.PhysicsRules.Data;
            double[] child = PhysicsRules.Data;
            int len = parent1.Length;
            var p1 = (int) (rnd.NextDouble()*len);
            var p2 = (int) (rnd.NextDouble()*len);

            for (int i = 0; i < PhysicsRules.Data.Length; i++)
            {
                if (i < p1)
                {
                    child[i] = parent1[i];
                }
                else if (i >= p1 && i <= p2)
                {
                    child[i] = parent2[i];
                }
                else if (i > p2)
                {
                    child[i] = parent1[i];
                }
            }
        }

        /// <summary>
        ///     Perform a mutate on a parent and generate a new child.  The parent is not changed.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <param name="sourcePhysics">The parent object.</param>
        /// <param name="probChange">The probability of changing an individual element.</param>
        /// <param name="perturb">The amount that an object is changed by.</param>
        public void Mutate(IGenerateRandom rnd,
            IPhysics sourcePhysics, double probChange,
            double perturb)
        {
            PhysicsRules.CopyData(sourcePhysics.Data);

            for (int i = 0; i < sourcePhysics.Data.Length; i++)
            {
                if (rnd.NextDouble() < probChange)
                {
                    PhysicsRules.Data[i] += perturb*rnd.NextDouble(-1, 1);
                }
            }
        }

        /// <summary>
        ///     Randomize the universe grid.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        public void Randomize(IGenerateRandom rnd)
        {
            lock (this)
            {
                _universe.Randomize(rnd);
                _iteration = 0;
            }
        }

        /// <summary>
        ///     Randomize the universe grid and the physics calculator.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        public void Reset(IGenerateRandom rnd)
        {
            lock (this)
            {
                _physics.Randomize();
                _universe.Randomize(rnd);
                _iteration = 0;
            }
        }


        /// <summary>
        /// </summary>
        /// <returns></returns>
        public override String ToString()
        {
            return "Iteration: " + _iteration + ", Diff=" + _diff;
        }
    }
}
