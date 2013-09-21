using System;
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.Distance;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Discrete
{
    [TestClass]
    public class TestDiscreteAnneal
    {
        [TestMethod]
        public void TestStatus()
        {
            DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
            Assert.AreEqual("k=0,kMax=1000,t=0,prob=0", anneal.Status);
        }

        [TestMethod]
        public void TestGeneral()
        {
            DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
            anneal.Cycles = 100;
            Assert.AreEqual(100, anneal.Cycles);
            Assert.AreEqual(0, anneal.K);
            Assert.AreEqual(false, anneal.Done);
        }

        [TestMethod]
        public void TestCoolingSchedule()
        {
            DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
            Assert.AreEqual(400, anneal.CoolingSchedule(), AIFH.DefaultPrecision);
            anneal.Iteration();
            Assert.AreEqual(397.61057939346017, anneal.CoolingSchedule(), AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestProbability()
        {
            DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
            Assert.AreEqual(0.9753099120283326, anneal.CalcProbability(10, 20, anneal.CoolingSchedule()), AIFH.DefaultPrecision);
            anneal.Iteration();
            Assert.AreEqual(0.9751633961486054, anneal.CalcProbability(10, 20, anneal.CoolingSchedule()), AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestRun()
        {
            DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
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
