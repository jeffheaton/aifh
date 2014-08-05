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
using AIFH_Vol2.Core.General;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning
{
    /// <summary>
    /// Iteratively trains a population by applying
    /// particle swarm optimisation (PSO).
    ///
    /// Based on Encog contribution by:
    /// Geoffroy Noel, https://github.com/goffer-looney
    ///
    /// References:
    /// James Kennedy and Russell C. Eberhart, Particle swarm optimization,
    /// Proceedings of the IEEE International Conference on Neural Networks,
    /// 1995, pp. 1942-1948
    ///
    /// </summary>
    public class TrainPSO : ILearningMethod
    {
        /**
     * The machine learning algorithm to optimize.
     */
        private readonly IMLMethod[] _particles;

        /**
         * The random number generator to use.
         */
        private IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /**
         * The current error.
         */
        private double currentError;

        /**
         * The scoring function, this determines the energy (error) of the current solution.
         */
        private readonly IScoreFunction _score;

        /**
         * Swarm state and memories.
         */
        protected double[][] _velocities;
        protected double[][] _bestVectors;
        protected double[] _bestScores;
        protected int _bestVectorIndex;

        /// <summary>
        /// Although this is redundant with m_bestVectors[bestVectorIndex],
        /// bestVectors[bestVectorIndex] is not thread safe.
        /// </summary>
        private double[] _bestVector;

 
        /// <summary>
        /// Determines the size of the search space.
        /// The position components of particle will be bounded to
        /// [-maxPos, maxPos]
        /// A well chosen range can improve the performance.
        /// -1 is a special value that represents boundless search space.
        /// </summary>
        protected double maxPosition = -1;

        /// <summary>
        /// Maximum change one particle can take during one iteration.
        /// Imposes a limit on the maximum absolute value of the velocity
        /// components of a particle.
        /// Affects the granularity of the search.
        /// If too high, particle can fly past optimum solution.
        /// If too low, particle can get stuck in local minima.
        /// Usually set to a fraction of the dynamic range of the search
        /// space (10% was shown to be good for high dimensional problems).
        /// -1 is a special value that represents boundless velocities.
        /// </summary>
        protected double maxVelocity = 2;

        /// <summary>
        /// c1, cognitive learning rate >= 0
        /// tendency to return to personal best position
        /// </summary>
        protected double c1 = 2.0;
        
        /// <summary>
        /// c2, social learning rate >= 0
        /// tendency to move towards the swarm best position
        /// </summary>
        protected double c2 = 2.0;

        /// <summary>
        /// The best score so far.
        /// </summary>
        private double bestScore;

        /// <summary>
        /// w, inertia weight.
        /// Controls global (higher value) vs local exploration
        /// of the search space.
        /// Analogous to temperature in simulated annealing.
        /// Must be chosen carefully or gradually decreased over time.
        /// Value usually between 0 and 1.
        /// </summary>
        protected double inertiaWeight = 0.4;

        /// <summary>
        /// Construct PSO trainer.
        /// </summary>
        /// <param name="theParticles">The particles to use.</param>
        /// <param name="theCalculateScore">The score object.</param>
        public TrainPSO(IMLMethod[] theParticles,
                        IScoreFunction theCalculateScore)
        {
            _particles = theParticles;
            _score = theCalculateScore;
            int vectorSize = theParticles[0].LongTermMemory.Length;
            int particleCount = theParticles.Length;

            _bestVectors = new double[particleCount][];
            _velocities = new double[particleCount][];
            _bestScores = new double[particleCount];

            for (int i = 0; i < particleCount; i++)
            {
                _bestVectors[i] = new double[vectorSize];
                _velocities[i] = new double[vectorSize];
            }

            _bestVectorIndex = -1;

            _bestVector = new double[vectorSize];

            foreach (double[] velocity in _velocities)
            {
                VectorAlgebra.Randomise(_rnd, velocity, this.maxVelocity);
            }
        }

        /// <summary>
        /// Update the velocity, position and personal
        /// best position of a particle. 
        /// </summary>
        /// <param name="particleIndex">index of the particle in the swarm</param>
        protected void UpdateParticle(int particleIndex)
        {

            double[] particlePosition = _particles[particleIndex].LongTermMemory;

            UpdateVelocity(particleIndex);

            // velocity clamping
            VectorAlgebra.ClampComponents(_velocities[particleIndex], maxVelocity);

            // new position (Xt = Xt-1 + Vt)
            VectorAlgebra.Add(particlePosition, _velocities[particleIndex]);

            // pin the particle against the boundary of the search space.
            // (only for the components exceeding maxPosition)
            VectorAlgebra.ClampComponents(particlePosition, maxPosition);

            UpdatePersonalBestPosition(particleIndex, particlePosition);
        }

        /**
         * Update the velocity of a particle
         *
         * @param particleIndex index of the particle in the swarm
         */
        protected void UpdateVelocity(int particleIndex)
        {
            double[] particlePosition = _particles[particleIndex].LongTermMemory;
            double[] vtmp = new double[particlePosition.Length];


            // Standard PSO formula

            // inertia weight
            VectorAlgebra.Mul(_velocities[particleIndex], inertiaWeight);

            // cognitive term
            VectorAlgebra.Copy(vtmp, _bestVectors[particleIndex]);
            VectorAlgebra.Sub(vtmp, particlePosition);
            VectorAlgebra.MulRand(_rnd, vtmp, this.c1);
            VectorAlgebra.Add(_velocities[particleIndex], vtmp);

            // social term
            if (particleIndex != _bestVectorIndex)
            {
                VectorAlgebra.Copy(vtmp, _bestVector);
                VectorAlgebra.Sub(vtmp, particlePosition);
                VectorAlgebra.MulRand(_rnd, vtmp, c2);
                VectorAlgebra.Add(_velocities[particleIndex], vtmp);
            }
        }


        /// <summary>
        /// Update the personal best position of a particle. 
        /// </summary>
        /// <param name="particleIndex">Index of the particle in the swarm.</param>
        /// <param name="particlePosition">the particle current position vector.</param>
        protected void UpdatePersonalBestPosition(int particleIndex, double[] particlePosition)
        {
            // set the network weights and biases from the vector
            double score = _score.CalculateScore(_particles[particleIndex]);

            // update the best vectors (g and i)
            if ((_bestScores[particleIndex] == 0) || IsScoreBetter(score, _bestScores[particleIndex]))
            {
                _bestScores[particleIndex] = score;
                VectorAlgebra.Copy(_bestVectors[particleIndex], particlePosition);
            }
        }

        /// <summary>
        /// Update the swarm's best position.
        /// </summary>
        protected void UpdateGlobalBestPosition()
        {
            bool bestUpdated = false;
            for (int i = 0; i < this._particles.Length; i++)
            {
                if ((_bestVectorIndex == -1) || IsScoreBetter(_bestScores[i], _bestScores[_bestVectorIndex]))
                {
                    _bestVectorIndex = i;
                    bestUpdated = true;
                }
            }
            if (bestUpdated)
            {
                VectorAlgebra.Copy(_bestVector, _bestVectors[_bestVectorIndex]);
                this.bestScore = _bestScores[_bestVectorIndex];
            }
        }

        
        /// <summary>
        /// Compares two scores. 
        /// </summary>
        /// <param name="score1">score1 a score</param>
        /// <param name="score2">score2 a score</param>
        /// <returns>true if score1 is better than score2</returns>
        bool IsScoreBetter(double score1, double score2)
        {
            return ((_score.ShouldMinimize && (score1 < score2)) || ((!_score.ShouldMinimize) && (score1 > score2)));
        }


        /// <inheritdoc/>
        public void Iteration()
        {
            for (int i = 0; i < _particles.Length; i++)
            {
                UpdateParticle(i);
            }

            UpdateGlobalBestPosition();
        }

        /// <summary>
        /// False, this algorithm can be iterated an unlimited number of times.
        /// </summary>
        public bool Done
        {
            get
            {
                return false;
            }
        }

        /// <summary>
        /// The error (or energy) from the last iteration.
        /// </summary>
        public double LastError
        {
            get
            {
                return this.bestScore;
            }
        }

        /// <summary>
        /// Calculate the probability that we will accept a move that takes us to a higher energy (higher error)
        /// position. 
        /// </summary>
        /// <param name="ecurrent">The current energy.</param>
        /// <param name="enew">The new energy if we move.</param>
        /// <param name="t">The current temperature.</param>
        /// <returns>The probability.</returns>
        public double CalcProbability(double ecurrent, double enew, double t)
        {
            return Math.Exp(-(Math.Abs(enew - ecurrent) / t));
        }

        /// <summary>
        /// Copy the global best solution to the machine learning algorithm.  
        /// It is very important to call this method.
        /// </summary>
        public void FinishTraining()
        {

        }

        /// <i>nheritdoc/      
        public string Status
        {
            get
            {
                return "";
            }
        }

        /// <summary>
        /// The best particle.
        /// </summary>
        public IMLMethod BestParticle
        {
            get
            {
                VectorAlgebra.Copy(_particles[_bestVectorIndex].LongTermMemory, _bestVector);
                return this._particles[_bestVectorIndex];
            }
        }
    }
}
