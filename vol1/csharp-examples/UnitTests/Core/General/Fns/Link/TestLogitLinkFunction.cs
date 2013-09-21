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
using AIFH_Vol1.Core.General.Fns.Link;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.General.Fns.Link
{
    [TestClass]
    public class TestLogitLinkFunction
    {
        [TestMethod]
        public void TestEvaluate()
        {
            var fn = new LogitLinkFunction();
            double[] x = { 2 };
            double y = fn.Evaluate(x);
            Assert.AreEqual(0.8807970779778823, y, AIFH.DefaultPrecision);
        }

        [TestMethod]
        [ExpectedException(typeof(AIFHError), "A 2D evaluation was allowed.")]
        public void TestException()
        {
            var fn = new LogitLinkFunction();
            double[] x = { 1, 2 };
            fn.Evaluate(x);
        }
    }
}
