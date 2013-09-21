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
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.KMeans;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.KMeans
{
    [TestClass]
    public class TestCluster
    {
        [TestMethod]
        public void TestDimensions()
        {
            var cluster = new Cluster(3);
            Assert.AreEqual(true, cluster.ToString().Length > 0);
            Assert.AreEqual(3, cluster.Dimensions);
        }

        [TestMethod]
        public void TestCenter()
        {
            var cluster = new Cluster(3);
            double[] ob1 = { 2.0, 10.0, 100.0 };
            double[] ob2 = { 4.0, 20.0, 200.0 };
            double[] ob3 = { 6.0, 30.0, 300.0 };

            cluster.Observations.Add(new BasicData(ob1));
            cluster.Observations.Add(new BasicData(ob2));
            cluster.Observations.Add(new BasicData(ob3));

            Assert.AreEqual(3, cluster.Observations.Count);

            cluster.CalculateCenter();

            Assert.AreEqual(4.0, cluster.Center[0], 0.00001);
            Assert.AreEqual(20.0, cluster.Center[1], 0.00001);
            Assert.AreEqual(200.0, cluster.Center[2], 0.00001);
        }
    }
}
