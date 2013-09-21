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
using AIFH_Vol1.Core.Distance;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Discrete
{
    /// <summary>
    /// Test discrete simulated annealing.
    /// </summary>
    [TestClass]
    public class TestDiscreteAnneal
    {
        [TestMethod]
        public void TestStatus()
        {
            var anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
            Assert.AreEqual("k=0,kMax=1000,t=0,prob=0", anneal.Status);
        }

        [TestMethod]
        public void TestGeneral()
        {
            var anneal = new DiscreteAnnealSubclass(1000, 4000, 1) {Cycles = 100};
            Assert.AreEqual(100, anneal.Cycles);
            Assert.AreEqual(0, anneal.K);
            Assert.AreEqual(false, anneal.Done);
        }

        [TestMethod]
        public void TestCoolingSchedule()
        {
            var anneal = new DiscreteAnnealSubclass(1000, 400, 1);
            Assert.AreEqual(400, anneal.CoolingSchedule(), AIFH.DefaultPrecision);
            anneal.Iteration();
            Assert.AreEqual(397.61057939346017, anneal.CoolingSchedule(), AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestProbability()
        {
            var anneal = new DiscreteAnnealSubclass(1000, 400, 1);
            Assert.AreEqual(0.9753099120283326, anneal.CalcProbability(10, 20, anneal.CoolingSchedule()), AIFH.DefaultPrecision);
            anneal.Iteration();
            Assert.AreEqual(0.9751633961486054, anneal.CalcProbability(10, 20, anneal.CoolingSchedule()), AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestRun()
        {
            var anneal = new DiscreteAnnealSubclass(1000, 400, 1);
            while (!anneal.Done)
            {
                anneal.Iteration();
            }

            ICalculateDistance dist = new EuclideanDistance();

            Assert.AreEqual(1000, anneal.K);
            Assert.AreEqual(0, dist.Calculate(anneal.Best, DiscreteAnnealSubclass.Ideal), AIFH.DefaultPrecision);
            Assert.AreEqual(0, anneal.BestScore, AIFH.DefaultPrecision);
        }
    }
}
