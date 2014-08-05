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
using System.Collections.Generic;
using System.IO;
using System.Text;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2.Examples.Capstone.Model.Milestone1;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone2
{
    /// <summary>
    ///     The second milestone for titanic is to fit and cross validate a model.
    /// </summary>
    public class FitTitanic
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Titanic Milestone 2: Fit the model.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 10;

        /// <summary>
        ///     The best RBF network.
        /// </summary>
        private RBFNetwork _bestNetwork;

        /// <summary>
        ///     The best score.
        /// </summary>
        private double _bestScore;

        /// <summary>
        ///     The cross validation folds.
        /// </summary>
        private CrossValidate _cross;

        /// <summary>
        /// The best network from the folds.
        /// </summary>
        public RBFNetwork BestNetwork
        {
            get { return _bestNetwork; }
        }

        /// <summary>
        /// The cross validation folds.
        /// </summary>
        public CrossValidate Crossvalidation
        {
            get { return _cross; }
        }

        /// <summary>
        /// Train one fold.
        /// </summary>
        /// <param name="k">The fold id.</param>
        /// <param name="fold">The fold.</param>
        public void TrainFold(int k, CrossValidateFold fold)
        {
            int noImprove = 0;
            double localBest = 0;

            // Get the training and cross validation sets.
            IList<BasicData> training = fold.TrainingSet;
            IList<BasicData> validation = fold.ValidationSet;

            // Create random particles for the RBF.
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();
            var particles = new RBFNetwork[TitanicConfig.ParticleCount];
            for (int i = 0; i < particles.Length; i++)
            {
                particles[i] = new RBFNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RbfCount, 1);
                particles[i].Reset(rnd);
            }

            /**
             * Construct a network to hold the best network.
             */
            if (_bestNetwork == null)
            {
                _bestNetwork = new RBFNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RbfCount, 1);
            }

            /**
             * Setup the scoring function.
             */
            IScoreFunction score = new ScoreTitanic(training);
            IScoreFunction scoreValidate = new ScoreTitanic(validation);

            /**
             * Setup particle swarm.
             */
            bool done = false;
            var train = new TrainPSO(particles, score);
            int iterationNumber = 0;
            var line = new StringBuilder();

            do
            {
                iterationNumber++;

                train.Iteration();

                var best = (RBFNetwork) train.BestParticle;

                double trainingScore = train.LastError;
                double validationScore = scoreValidate.CalculateScore(best);

                if (validationScore > _bestScore)
                {
                    Array.Copy(best.LongTermMemory, 0, _bestNetwork.LongTermMemory, 0, best.LongTermMemory.Length);
                    _bestScore = validationScore;
                }

                if (validationScore > localBest)
                {
                    noImprove = 0;
                    localBest = validationScore;
                }
                else
                {
                    noImprove++;
                }

                line.Length = 0;
                line.Append("Fold #");
                line.Append(k + 1);
                line.Append(", Iteration #");
                line.Append(iterationNumber);
                line.Append(": training correct: ");
                line.Append(trainingScore);
                line.Append(", validation correct: ");
                line.Append(validationScore);
                line.Append(", no improvement: ");
                line.Append(noImprove);

                if (noImprove > TitanicConfig.AllowNoImprovement)
                {
                    done = true;
                }

                Console.WriteLine(line.ToString());
            } while (!done);

            fold.Score = localBest;
        }

        /// <summary>
        /// Fit a RBF model to the titanic. 
        /// </summary>
        /// <param name="dataPath">The path that contains the data file.</param>
        public void Process(string dataPath)
        {
            string trainingPath = Path.Combine(dataPath, TitanicConfig.TrainingFilename);
            string testPath = Path.Combine(dataPath, TitanicConfig.TestFilename);

            IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

            // Generate stats on the titanic.
            var stats = new TitanicStats();
            NormalizeTitanic.Analyze(stats, trainingPath);
            NormalizeTitanic.Analyze(stats, testPath);

            // Get the training data for the titanic.
            IList<BasicData> training = NormalizeTitanic.Normalize(stats, trainingPath, null,
                TitanicConfig.InputNormalizeLow,
                TitanicConfig.InputNormalizeHigh,
                TitanicConfig.PredictSurvive,
                TitanicConfig.PredictPerish);

            // Fold the data for cross validation.
            _cross = new CrossValidate(TitanicConfig.FoldCount, training, rnd);

            // Train each of the folds.
            for (int k = 0; k < _cross.Count; k++)
            {
                Console.WriteLine("Cross validation fold #" + (k + 1) + "/" + _cross.Count);
                TrainFold(k, _cross.Folds[k]);
            }

            // Show the cross validation summary.
            Console.WriteLine("Crossvalidation summary:");
            int kk = 1;
            foreach (CrossValidateFold fold in _cross.Folds)
            {
                Console.WriteLine("Fold #" + kk + ": " + fold.Score);
                kk++;
            }

            Console.WriteLine("Final, crossvalidated score:" + _cross.Score);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            string filename = "";

            if (args.Length > 0)
            {
                filename = args[0];
                string dataPath = filename;

                var fit = new FitTitanic();
                fit.Process(dataPath);
            }
            else
            {
                Console.WriteLine("Please provide your data directory path as the first argument.");
            }
        }
    }
}
