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
using AIFH_Vol1.Core.General.Fns.Link;
using AIFH_Vol1.Core.Regression;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Regression
{
    [TestClass]
    public class TestTrainReweightLeastSquares
    {
        [TestMethod]
        public void TestTrain()
        {

            double[][] x = {
                new [] {1.0},
                new [] {3.0},
                new [] {2.0},
                new [] {200.0},
                new [] {230.0}};

            double[][] y = {
                new [] {1.0},
                new [] {1.0},
                new [] {1.0},
                new [] {0.0},
                new [] {0.0}
        };


            var trainingData = BasicData.ConvertArrays(x, y);
            var regression = new MultipleLinearRegression(1) {LinkFunction = new LogitLinkFunction()};
            var train = new TrainReweightLeastSquares(regression, trainingData);
            train.Iteration();

            double[] input = { 0 };
            double[] output = regression.ComputeRegression(input);
            Assert.AreEqual(0.883301730269988, output[0], AIFH.DefaultPrecision);
        }
    }
}
