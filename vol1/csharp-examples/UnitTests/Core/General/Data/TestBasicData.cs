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

using System.Collections.Generic;
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.General.Data;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.General.Data
{
    [TestClass]
    public class TestBasicData
    {

        public static readonly double[][] TestInput = {
            new[]{0.0, 0.0},
            new[]{1.0, 0.0},
            new[]{0.0, 1.0},
            new[]{1.0, 1.0}
    };

        public static readonly double[][] TestIdeal = {
            new[]{0.0},
            new[]{1.0},
            new[]{1.0},
            new[]{0.0}
    };

        [TestMethod]
        public void TestUnSupervised()
        {
            var data = new BasicData(2);
            Assert.AreEqual(2, data.Input.Length);
            Assert.AreEqual(0, data.Ideal.Length);
        }
        [TestMethod]
        public void TestSupervised()
        {
            var data = new BasicData(2, 1);
            Assert.AreEqual(2, data.Input.Length);
            Assert.AreEqual(1, data.Ideal.Length);
        }
        [TestMethod]
        public void TestLabel()
        {
            var data = new BasicData(2) {Label = "label"};
            Assert.AreEqual("label", data.Label);
        }
        [TestMethod]
        public void TestToString()
        {
            var data = new BasicData(2);
            Assert.AreEqual("[BasicData: input:0,0, ideal:, label:]", data.ToString());
        }
        [TestMethod]
        public void TestArray()
        {
            double[] a = { 1.0, 2.0 };
            var d = new BasicData(a);
            Assert.AreEqual(2, d.Input.Length);
            Assert.AreEqual(0, d.Ideal.Length);

            Assert.AreEqual(1.0, d.Input[0], AIFH.DefaultPrecision);
            Assert.AreEqual(2.0, d.Input[1], AIFH.DefaultPrecision);

        }
        [TestMethod]
        public void TestArrays()
        {
            IList<BasicData> list = BasicData.ConvertArrays(TestInput, TestIdeal);
            Assert.AreEqual(4, list.Count);
            Assert.AreEqual(2, list[0].Input.Length);
            Assert.AreEqual(1, list[0].Ideal.Length);

            Assert.AreEqual(1, list[1].Input[0], AIFH.DefaultPrecision);
            Assert.AreEqual(0, list[1].Input[1], AIFH.DefaultPrecision);
            Assert.AreEqual(1, list[1].Ideal[0], AIFH.DefaultPrecision);
        }
    }
}
