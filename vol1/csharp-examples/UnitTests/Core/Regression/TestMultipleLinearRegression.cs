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
using AIFH_Vol1.Core.Regression;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Regression
{
    [TestClass]
    public class TestMultipleLinearRegression
    {
        [TestMethod]
        public void TestBasic()
        {
            var reg = new MultipleLinearRegression(1);

            Assert.AreEqual(2, reg.LongTermMemory.Length);

            var lnk = new LogLinkFunction();
            reg.LinkFunction = lnk;
            Assert.IsTrue(reg.LinkFunction == lnk);

            reg.LongTermMemory[0] = 1;
            reg.LongTermMemory[1] = 2;

            double[] input = { 1.0 };
            double[] output = reg.ComputeRegression(input);
            Assert.AreEqual(1, output.Length);
            Assert.AreEqual(1.0986122886681098, output[0], AIFH.DefaultPrecision);
        }
    }
}
