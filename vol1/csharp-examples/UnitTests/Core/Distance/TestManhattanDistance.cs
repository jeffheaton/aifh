using AIFH_Vol1.Core.Distance;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Distance
{
    [TestClass]
    public class TestManhattanDistance
    {
        [TestMethod]
        public void TestDistanceCalc()
        {
            ICalculateDistance calc = new ManhattanDistance();
            double[] pos1 = { 0.5, 1.0, 2.5 };
            double[] pos2 = { 0.1, 2.0, -2.5 };

            Assert.AreEqual(6.4, calc.Calculate(pos1, pos2), 0.001);
        }
    }
}
