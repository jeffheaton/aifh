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
using System.Linq;
using AIFH_Vol1.Core;
using AIFH_Vol1.Core.Learning;
using AIFH_Vol1.Core.Randomize;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Learning
{
    [TestClass]
    public class TestRBFNetwork
    {
        [TestMethod]
        public void TestBasics()
        {
            var network = new RBFNetwork(2, 1, 1);

            // should be 7, (2*1) + (1+(1 bias))*1 + 3 RBF params
            // 2 + 2 + 3 = 7
            Assert.AreEqual(7, network.LongTermMemory.Length);

            Assert.AreEqual("[RBFNetwork:inputCount=2,outputCount=1,RBFs=[GaussianFunction:width=0.00,center=0.00,0.00],]", network.ToString());

        }

        [TestMethod]
        public void TestResetCompute()
        {
            var network = new RBFNetwork(2, 1, 1);
            double total = network.LongTermMemory.Sum();
            Assert.AreEqual(0, total, AIFH.DefaultPrecision);

            network.Reset(new BasicGenerateRandom());

            total += network.LongTermMemory.Sum();

            Assert.IsTrue(Math.Abs(total) > AIFH.DefaultPrecision);

        }

        [TestMethod]
        public void TestComputeRegression()
        {
            var network = new RBFNetwork(2, 1, 1);

            double[] ltm = {
                2.0,  // input 1 to RBF 1
                2.0,  // input 2 to RBF 1
                5.0,  // RBF width
                2.0,  // RBF, center-0
                4.0,  // RBF, center-1
                3.0,  // RBF1 to Output 1
                4.0};  // Bias to Output 1


            Array.Copy(ltm, 0, network.LongTermMemory, 0, ltm.Length);

            double[] x = { 1, 2 };

            double y = network.ComputeRegression(x)[0];

            // Inputs: (2*1) + (2*2) = 6
            // RBF: Gaussian(6) = 1
            // Outputs: (1*3) + (1*4) = 7
            Assert.AreEqual(7, y, AIFH.DefaultPrecision);
        }

        [TestMethod]
        public void TestComputeClassification()
        {
            var network = new RBFNetwork(2, 1, 2);

            double[] ltm = {
                2.0,  // input 1 to RBF 1
                2.0,  // input 2 to RBF 1
                5.0,  // RBF width
                2.0,  // RBF, center-0
                4.0,  // RBF, center-1
                3.0,  // RBF1 to Output 1
                4.0,  // Bias to Output 1
                5.0,  // RBF1 to Output 2
                6.0}; // Bias to Output 2


            Array.Copy(ltm, 0, network.LongTermMemory, 0, ltm.Length);

            double[] x = { 1, 2 };

            double[] y = network.ComputeRegression(x);

            // Inputs: (2*1) + (2*2) = 6
            // RBF: Gaussian(6) = 1
            // Outputs: (1*3) + (1*4) = 7
            Assert.AreEqual(7, y[0], AIFH.DefaultPrecision);

            // Inputs: (2*1) + (2*2) = 6
            // RBF: Gaussian(6) = 1
            // Outputs: (1*5) + (1*6) = 11
            Assert.AreEqual(11, y[1], AIFH.DefaultPrecision);

            int cls = network.ComputeClassification(x);

            // class 1 is higher than class 0
            Assert.AreEqual(1, cls);
        }
    }
}
