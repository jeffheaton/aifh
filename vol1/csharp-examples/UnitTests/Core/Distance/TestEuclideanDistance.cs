using System;
using System.Text;
using System.Collections.Generic;
using AIFH_Vol1.Core.Distance;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Distance
{
    /// <summary>
    /// Summary description for TestEuclideanDistance
    /// </summary>
    [TestClass]
    public class TestEuclideanDistance
    {
        [TestMethod]
        public void TestDistanceCalc()
        {
            ICalculateDistance calc = new EuclideanDistance();
            double[] pos1 = { 0.5, 1.0, 2.5, };
            double[] pos2 = { 0.1, 2.0, -2.5, };

            Assert.AreEqual(5.1146, calc.Calculate(pos1, pos2), 0.001);
        }
    }
}
