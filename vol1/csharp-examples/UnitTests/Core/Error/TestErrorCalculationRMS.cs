using System;
using AIFH_Vol1.Core.Error;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Error
{
    [TestClass]
    public class TestErrorCalculationRMS
    {
        [TestMethod]
        public void TestErrorCalc()
        {
            IErrorCalculation calc = new ErrorCalculationRMS();
            double result = ErrorTestingUtil.CalculateError(
                    calc,
                    ErrorTestingUtil.ACTUAL,
                    ErrorTestingUtil.IDEAL);
            Assert.AreEqual(12.3134, result, 0.001);
        }
    }
}
