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
using System;
using System.Collections.Generic;
using System.IO;
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Normalize;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Normalize
{
    [TestClass]
    public class TestDataSet
    {
        private DataSet generateTestData()
        {
            String[] headers = { "text", "numeric", "dec" };
            var ds = new DataSet(headers);

            Object[] row1 = { "One", "1", "0.1" };
            Object[] row2 = { "Two", "2", "0.2" };
            Object[] row3 = { "Three", "3", "0.3" };

            ds.Add(row1);
            ds.Add(row2);
            ds.Add(row3);

            return ds;
        }

        [TestMethod]
        public void TestLoadSave()
        {
            DataSet ds = generateTestData();
            DataSet.Save("~deleteme.tmp", ds);
            DataSet dataset2 = DataSet.Load("~deleteme.tmp");
            File.Delete("~deleteme.tmp");

            Assert.IsTrue(ds.Equals(dataset2));
            Assert.IsTrue(dataset2.Equals(ds));

            Assert.AreEqual(3, ds.Count);
            Assert.AreEqual(3, ds.HeaderCount);
        }

        [TestMethod]
        public void TestEqual()
        {
            DataSet ds1 = generateTestData();
            DataSet ds2 = generateTestData();
            Assert.IsTrue(ds1.Equals(ds2));
        }

        [TestMethod]
        public void TestNotEqualHeaders()
        {
            DataSet ds1 = generateTestData();
            DataSet ds2 = generateTestData();

            ds1.Headers[1] = "--";

            Assert.IsFalse(ds1.Equals(ds2));
        }

        [TestMethod]
        public void TestNotEqualHeaderCount()
        {
            DataSet ds1 = generateTestData();
            DataSet ds2 = generateTestData();
            ds1.AppendColumns(1);
            Assert.IsFalse(ds1.Equals(ds2));
        }

        [TestMethod]
        public void TestNotEqualRowCount()
        {
            DataSet ds1 = generateTestData();
            DataSet ds2 = generateTestData();
            ds1.Data.RemoveAt(0);
            Assert.IsFalse(ds1.Equals(ds2));
        }

        [TestMethod]
        public void TestNotEqualRows()
        {
            DataSet ds1 = generateTestData();
            DataSet ds2 = generateTestData();
            ds1.Data[0][0] = "---";
            Assert.IsFalse(ds1.Equals(ds2));
        }

        [TestMethod]
        public void TestNotEqualOtherObject()
        {
            DataSet ds1 = generateTestData();
            Assert.IsFalse(ds1.Equals(""));
        }

        [TestMethod]
        public void TestMin()
        {
            DataSet ds1 = generateTestData();
            Assert.AreEqual(1.0, ds1.GetMin(1), AIFH.DefaultPrecision);
            // test again, as strings are now numbers, from the last call
            Assert.AreEqual(1.0, ds1.GetMin(1), AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestMax()
        {
            DataSet ds1 = generateTestData();
            Assert.AreEqual(3.0, ds1.GetMax(1), AIFH.DefaultPrecision);
            // test again, as strings are now numbers, from the last call
            Assert.AreEqual(3.0, ds1.GetMax(1), AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestNormalizeRange()
        {
            DataSet ds1 = generateTestData();
            ds1.NormalizeRange(1, -1, 1);
            Assert.AreEqual(-1.0, double.Parse(ds1.Data[0][1].ToString())
                    , AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestDeNormalizeRange()
        {
            DataSet ds1 = generateTestData();

            double min = ds1.GetMin(2);
            double max = ds1.GetMax(2);

            ds1.NormalizeRange(2, min, max, -1, 1);
            Assert.AreEqual(-1.0, double.Parse(ds1.Data[0][2].ToString())
                    , AIFH.DefaultPrecision);
            ds1.DeNormalizeRange(2, min, max, -1, 1);
            Assert.AreEqual(0.1, double.Parse(ds1.Data[0][2].ToString())
                    , AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestNormalizeReciprocal()
        {
            DataSet ds1 = generateTestData();
            ds1.NormalizeReciprocal(1);
            Assert.AreEqual(0.5, double.Parse(ds1.Data[1][1].ToString())
                    , AIFH.DefaultPrecision);
            ds1.DeNormalizeReciprocal(1);
            Assert.AreEqual(2.0, double.Parse(ds1.Data[1][1].ToString())
                    , AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestEncodeNumeric()
        {
            DataSet ds1 = generateTestData();
            ds1.EncodeNumeric(0);
        }

        [TestMethod]
        public void TestEncodeOneOfN()
        {
            DataSet ds1 = generateTestData();
            ds1.EncodeOneOfN(0);
        }

        [TestMethod]
        public void TestEncodeEquilateral()
        {
            DataSet ds1 = generateTestData();
            ds1.EncodeEquilateral(0);
        }

        [TestMethod]
        public void TestDeleteColumn()
        {
            DataSet ds1 = generateTestData();
            ds1.DeleteColumn(0);
            Assert.AreEqual(2, ds1.HeaderCount);
            Assert.IsTrue(ds1.Headers[0].Equals("numeric"));
            Assert.IsTrue(ds1.Headers[1].Equals("dec"));
        }

        [TestMethod]
        public void TestExtractUnsupervisedLabeled()
        {
            DataSet ds1 = generateTestData();
            IList<BasicData> result = ds1.ExtractUnsupervisedLabeled(0);
            Assert.AreEqual(3, result.Count);
            Assert.IsTrue(result[0].Label.Equals("One"));
        }

        [TestMethod]
        public void TestExtractSupervised()
        {
            DataSet ds1 = generateTestData();
            IList<BasicData> result = ds1.ExtractSupervised(1, 1, 2, 1);
            Assert.AreEqual(3, result.Count);
        }

        [TestMethod]
        public void TestReplaceColumn()
        {
            DataSet ds1 = generateTestData();
            ds1.ReplaceColumn(1, 2, 1, 0);
            IList<BasicData> result = ds1.ExtractSupervised(1, 1, 2, 1);
            Assert.AreEqual(0.0, result[0].Input[0], AIFH.DefaultPrecision);
            Assert.AreEqual(1.0, result[1].Input[0], AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestDeleteUnknowns()
        {
            DataSet ds1 = generateTestData();
            ds1.Data[1][2] = "?";
            ds1.DeleteUnknowns();
            Assert.AreEqual(2, ds1.Data.Count);
        }
    }
}
