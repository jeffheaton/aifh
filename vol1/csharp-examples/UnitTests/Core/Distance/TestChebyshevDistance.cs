using AIFH_Vol1.Core.Distance;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Distance
{
    /// <summary>
    /// Test the Chebyshev distance calculation.
    /// </summary>
    [TestClass]
    public class TestChebyshevDistance
    {
        [TestMethod]
        public void TestDistanceCalc()
        {
            ICalculateDistance calc = new ChebyshevDistance();
            double[] pos1 = {0.5, 1.0, 2.5};
            double[] pos2 = {0.1, 2.0, -2.5};

            Assert.AreEqual(5.0, calc.Calculate(pos1, pos2), 0.001);
        }
    }
}