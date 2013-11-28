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
using AIFH_Vol1.Core.General.Fns;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.General.Fns
{
    [TestClass]
    public class TestMultiquadricFunction
    {
        [TestMethod]
        public void TestEvaluate()
        {
            double[] ps = { 5, 0, 0, 0 };
            var funct = new MultiquadricFunction(3, ps, 0);
            double[] x = { -1, 0, 1 };
            double y = funct.Evaluate(x);
            Assert.AreEqual(8.774964387392123, y, AIFH.DefaultPrecision);

        }

        [TestMethod]
        public void TestToString()
        {
            double[] ps = { 5, 0, 0, 0 };
            var funct = new MultiquadricFunction(3, ps, 0);
            double[] x = { -1, 0, 1 };
            funct.Evaluate(x);
            Assert.AreEqual("[MultiquadricFunction:width=5.00,center=0.00,0.00,0.00]", funct.ToString());
        }

        [TestMethod]
        public void TestOther()
        {
            double[] ps = { 5, 0, 0, 0 };
            var funct = new MultiquadricFunction(3, ps, 0);
            Assert.AreEqual(3, funct.Dimensions);
            funct.SetCenter(0, 100);
            Assert.AreEqual(100, funct.GetCenter(0), AIFH.DefaultPrecision);
            funct.Width = 5;
            Assert.AreEqual(5, funct.Width, AIFH.DefaultPrecision);
        }
    }
}
