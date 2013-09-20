using AIFH_Vol1.Core.Error;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Error
{
    [TestClass]
    public class TestErrorCalculationESS
    {
        [TestMethod]
        public void TestErrorCalc()
        {
            IErrorCalculation calc = new ErrorCalculationESS();
            double result = ErrorTestingUtil.CalculateError(
                    calc,
                    ErrorTestingUtil.ACTUAL,
                    ErrorTestingUtil.IDEAL);
            Assert.AreEqual(1516.205, result, 0.001);
        }
    }
}
