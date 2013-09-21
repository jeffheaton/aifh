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
using System;
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.Learning;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Learning
{
    [TestClass]
    public class TestTraining
    {
        private void PerformTest(ILearningMethod train)
        {

            Assert.IsFalse(train.Done);

            Assert.IsNotNull(train.Status);
            train.Iteration();
            double startError = train.LastError;

            for (int i = 0; i < 1000 && !train.Done; i++)
            {
                train.Iteration();
            }

            // make sure one last iteration does not blow up(if done was true)
            train.Iteration();

            train.FinishTraining();
            Assert.IsTrue((train.LastError < startError) || Math.Abs(train.LastError) < 1);
        }


        [TestMethod]
        public void TestAnneal()
        {
            var anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
            PerformTest(anneal);
        }

        [TestMethod]
        public void TestGreedyRandom()
        {
            var train = new TrainGreedyRandom(true, new TrialAlgo(), new TrialScore()) {LowRange = 0, HighRange = 10};

            Assert.AreEqual(0, train.LowRange, AIFH.DefaultPrecision);
            Assert.AreEqual(10, train.HighRange, AIFH.DefaultPrecision);

            PerformTest(train);
        }

        [TestMethod]
        public void TestHillClimbing()
        {
            var train = new TrainHillClimb(true, new TrialAlgo(), new TrialScore());
            PerformTest(train);
        }

        [TestMethod]
        public void TestNelderMead()
        {
            var train = new TrainNelderMead(new TrialAlgo(), new TrialScore());
            PerformTest(train);
        }
    }
}
