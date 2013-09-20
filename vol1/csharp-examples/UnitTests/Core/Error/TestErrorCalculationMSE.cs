using System;
using AIFH_Vol1.Core.Error;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Error
{
    [TestClass]
    public class TestErrorCalculationMSE
    {
        [TestMethod]
        public void TestErrorCalc()
        {
            IErrorCalculation calc = new ErrorCalculationMSE();
            double result = ErrorTestingUtil.CalculateError(
                    calc,
                    ErrorTestingUtil.ACTUAL,
                    ErrorTestingUtil.IDEAL);
            Assert.AreEqual(151.6205, result, 0.001);
        }
    }
}
