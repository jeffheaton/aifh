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
using AIFH_Vol1.Core.Distance;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.KMeans;
using AIFH_Vol1.Core.Randomize;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.KMeans
{
    [TestClass]
    public class TestKMeans
    {
        public static IList<BasicData> GetDataSet()
        {
            IList<BasicData> result = new List<BasicData>();
            result.Add(new BasicData(new double[] { 0, 0 }, "a"));
            result.Add(new BasicData(new double[] { 0, 1 }, "a"));
            result.Add(new BasicData(new double[] { 1, 0 }, "a"));
            result.Add(new BasicData(new double[] { 100, 100 }, "b"));
            result.Add(new BasicData(new double[] { 99, 100 }, "b"));
            result.Add(new BasicData(new double[] { 100, 99 }, "b"));
            result.Add(new BasicData(new double[] { 0, 100 }, "c"));
            result.Add(new BasicData(new double[] { 1, 100 }, "c"));
            result.Add(new BasicData(new double[] { 0, 99 }, "c"));
            result.Add(new BasicData(new double[] { 100, 0 }, "d"));
            result.Add(new BasicData(new double[] { 100, 1 }, "d"));
            result.Add(new BasicData(new double[] { 99, 0 }, "d"));


            return result;
        }

        [TestMethod]
        public void TestClusterForgy()
        {
            var kmeans = new KMeansClustering(4) {RandomGeneration = new BasicGenerateRandom(22)};
            kmeans.InitForgy(GetDataSet());
            int iterations = kmeans.Iteration(1000);
            Assert.AreEqual(2, iterations);

            Cluster cluster1 = kmeans.Clusters[0];
            Cluster cluster2 = kmeans.Clusters[1];
            Cluster cluster3 = kmeans.Clusters[2];
            Cluster cluster4 = kmeans.Clusters[3];

            Assert.AreEqual(3, cluster1.Observations.Count);
            Assert.AreEqual(3, cluster2.Observations.Count);
            Assert.AreEqual(3, cluster3.Observations.Count);
            Assert.AreEqual(3, cluster4.Observations.Count);
        }

        [TestMethod]
        public void TestClusterRandom()
        {
            var kmeans = new KMeansClustering(4) {RandomGeneration = new BasicGenerateRandom(44)};
            kmeans.InitRandom(GetDataSet());
            int iterations = kmeans.Iteration(1000);
            Assert.AreEqual(2, iterations);

            Cluster cluster1 = kmeans.Clusters[0];
            Cluster cluster2 = kmeans.Clusters[1];
            Cluster cluster3 = kmeans.Clusters[2];
            Cluster cluster4 = kmeans.Clusters[3];

            Assert.AreEqual(3, cluster1.Observations.Count);
            Assert.AreEqual(1, cluster2.Observations.Count);
            Assert.AreEqual(6, cluster3.Observations.Count);
            Assert.AreEqual(2, cluster4.Observations.Count);
        }

        [TestMethod]
        public void TestGeneral()
        {
            var kmeans = new KMeansClustering(5);
            Assert.AreEqual(5, kmeans.K);
            kmeans.RandomGeneration = new BasicGenerateRandom();
            kmeans.DistanceMetric = new EuclideanDistance();
        }

        [TestMethod]
        [ExpectedException(typeof(AIFHError), "Too many clusters allowed")]
        public void TestTooManyClusters()
        {
            var kmeans = new KMeansClustering(13);
            kmeans.InitRandom(GetDataSet());
        }

        [TestMethod]
        [ExpectedException(typeof(AIFHError), "Iteration too early allowed")]
        public void TestEarlyIteration()
        {
            var kmeans = new KMeansClustering(3);
            kmeans.Iteration();
        }

        [TestMethod]
        [ExpectedException(typeof(AIFHError), "No observations allowed")]
        public void TestNoObservations()
        {
            IList<BasicData> list = new List<BasicData>();
            var kmeans = new KMeansClustering(3);
            kmeans.InitForgy(list);
        }

        [TestMethod]
        [ExpectedException(typeof(AIFHError), "No dimenaions allowed")]
        public void TestNoDimension()
        {
            IList<BasicData> list = new List<BasicData>();
            list.Add(new BasicData(0));
            var kmeans = new KMeansClustering(3);
            kmeans.InitForgy(list);
        }

        [TestMethod]
        public void TestMaxClusters()
        {
            var kmeans = new KMeansClustering(12);
            kmeans.RandomGeneration = new BasicGenerateRandom(22);
            kmeans.InitRandom(GetDataSet());
            int iterations = kmeans.Iteration(1000);
            Assert.AreEqual(1, iterations);
        }
    }
}
