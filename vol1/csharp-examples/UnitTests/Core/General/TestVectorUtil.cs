using System;
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.General;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.General
{
    [TestClass]
    public class TestVectorUtil
    {

        public void TestMaxIndex()
        {
            double[] a = { 2, 4, 10, 8 };
            Assert.AreEqual(2, VectorUtil.MaxIndex(a), AIFH.DefaultPrecision);
        }

    }
}
