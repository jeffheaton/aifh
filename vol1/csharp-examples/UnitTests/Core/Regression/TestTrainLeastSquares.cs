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
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Regression;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Regression
{
    [TestClass]
    public class TestTrainLeastSquares
    {
        [TestMethod]
        public void TestTrain()
        {

            double[][] x = {
                new [] {5.0, 10.0, 2.0},
                new []{10.0, 20.0, 4.0},
                new []{15.0, 30.0, 6.0},
                new []{20.0, 40.0, 8.0},
                new []{25.0, 50.0, 10.0}};

            double[][] y = {
                new []{70.0},
                new []{132.0},
                new []{194.0},
                new []{256.0},
                new []{318.0}
        };


            var trainingData = BasicData.ConvertArrays(x, y);
            var regression = new MultipleLinearRegression(3);
            var train = new TrainLeastSquares(regression, trainingData);
            train.Iteration();

            Assert.AreEqual(8, regression.LongTermMemory[0], 0.0001);
            Assert.AreEqual(-54.8, regression.LongTermMemory[1], 0.0001);
            Assert.AreEqual(8, regression.LongTermMemory[2], 0.0001);
            Assert.AreEqual(1, train.R2, 0.0001);
            Assert.AreEqual(0, train.Error, AIFH.DefaultPrecision);

            for (int i = 0; i < x.Length; i++)
            {
                double[] output = regression.ComputeRegression(x[i]);
                Assert.AreEqual(y[i][0], output[0], 0.0001);
            }
        }
    }
}
