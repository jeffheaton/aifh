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
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.Learning;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Learning
{
    [TestClass]
    public class TestLearnAnneal
    {
        [TestMethodAttribute]
        public void TestBasic()
        {
            var anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
            Assert.AreEqual(400, anneal.CoolingSchedule(), AIFH.DefaultPrecision);
        }

        [TestMethodAttribute]
        public void TestGetStatus()
        {
            var anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
            Assert.AreEqual("k=0,kMax=1000,t=0,prob=0", anneal.Status);
        }

        [TestMethod]
        public void TestRandomize()
        {
            var algo = new TrialAlgo();
            var anneal = new TrainAnneal(algo, new TrialScore());
            anneal.PerformRandomize(algo.LongTermMemory);
            anneal.FinishTraining();
            Assert.AreEqual(0, algo.LongTermMemory[0], AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestIterations()
        {
            var anneal = new TrainAnneal(new TrialAlgo(), new TrialScore(), 10, 400, 0.0001) {Cycles = 10};
            Assert.AreEqual(400, anneal.CoolingSchedule(), AIFH.DefaultPrecision);


            Assert.AreEqual(400, anneal.StartingTemperature, AIFH.DefaultPrecision);
            Assert.AreEqual(0.0001, anneal.EndingTemperature, AIFH.DefaultPrecision);
            Assert.AreEqual(10, anneal.Cycles);

            Assert.AreEqual(0, anneal.CurrentTemperature, AIFH.DefaultPrecision);
            Assert.AreEqual(0, anneal.K);
            Assert.AreEqual(false, anneal.Done);
            Assert.AreEqual(true, double.IsInfinity(anneal.LastError));
            Assert.AreEqual(0, anneal.LastProbability, AIFH.DefaultPrecision);
            anneal.Iteration();

            Assert.AreEqual(true, anneal.LastError > 0);

            Assert.AreEqual(87.46896591546223, anneal.CurrentTemperature, AIFH.DefaultPrecision);
            Assert.AreEqual(1, anneal.K);
            Assert.AreEqual(false, anneal.Done);

            for (int i = 0; i < 9; i++)
            {
                anneal.Iteration();
            }

            Assert.AreEqual(true, anneal.Done);
            Assert.AreEqual(9.999999999999E-5, anneal.CurrentTemperature, AIFH.DefaultPrecision);
            Assert.AreEqual(10, anneal.K);

        }
    }
}
