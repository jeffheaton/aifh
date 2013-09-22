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
using AIFH_Vol1.Core.Normalize;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Normalize
{
    [TestClass]
    public class TestEquilateral
    {
        [TestMethod]
        [ExpectedException(typeof(AIFHError), "Too few classes allowed")]
        public void TestTooFew()
        {
            new Equilateral(2, -1, 1);
        }

        [TestMethod]
        public void TestEncode()
        {
            var eq = new Equilateral(3, -1, 1);
            double[] d = eq.Encode(1);
            Assert.AreEqual(0.8660254037844386, d[0], AIFH.DefaultPrecision);
            Assert.AreEqual(-0.5, d[1], AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestDecode()
        {
            var eq = new Equilateral(3, -1, 1);
            double[] d0 = { 0.866, 0.5 };
            double[] d1 = { -0.866, 0.5 };
            double[] d2 = { 0, -1 };
            Assert.AreEqual(2, eq.Decode(d0));
            Assert.AreEqual(2, eq.Decode(d1));
            Assert.AreEqual(0, eq.Decode(d2));
        }

        [TestMethod]
        [ExpectedException(typeof(AIFHError), "Too many allowed")]
        public void TestError()
        {
            var eq = new Equilateral(3, -1, 1);
            eq.Encode(10);
        }
    }
}
