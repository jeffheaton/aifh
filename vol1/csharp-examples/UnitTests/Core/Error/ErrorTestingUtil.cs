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

using AIFH_Vol1.Core.Error;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests.Core.Error
{
    public class ErrorTestingUtil
    {
        public static double[][] Ideal = {
            new [] {1.0, 2.0, 3.0, 4.0},
            new [] {5.0, 6.0, 7.0, 8.0},
            new [] {9.0, 10.0, 11.0, 12.0},
            new [] {13.0, 14.0, 15.0, 16.0},
            new [] {17.0, 18.0, 19.0, 20.0}
    };

        public static double[][] Actual = {
            new [] {1.1, -2.0, -3.0, 4.1},
            new [] {-5.1, -6.0, 7.1, 8.2},
            new [] {9.1, 10.2, -11.5, 12.1},
            new [] {13.0, -14.0, 15.0, 16.1},
            new [] {17.0, 18.0, -19.0, 20.1}
    };

        public static double CalculateError(IErrorCalculation calc, double[][] actual, double[][] ideal)
        {

            // First we are going to calculate by passing in 1d arrays to
            // the error calculation.  This is the most common case.

            calc.Clear();

            Assert.AreEqual(double.PositiveInfinity, calc.Calculate(), 0.0001);

            for (int i = 0; i < actual.Length; i++)
            {
                double[] actualData = actual[i];
                double[] idealData = ideal[i];
                calc.UpdateError(actualData, idealData, 1.0);
            }
            Assert.AreEqual(20, calc.SetSize);
            double error1 = calc.Calculate();

            // Secondly we are going to calculate by passing individual
            // elements.  This is less common, but the error calculation
            // should result in the same as above.

            calc.Clear();

            Assert.AreEqual(double.PositiveInfinity, calc.Calculate(), 0.0001);

            for (int i = 0; i < actual.Length; i++)
            {
                double[] actualData = actual[i];
                double[] idealData = ideal[i];
                for (int j = 0; j < actualData.Length; j++)
                {
                    calc.UpdateError(actualData[j], idealData[j]);
                }
            }
            Assert.AreEqual(20, calc.SetSize);
            double error2 = calc.Calculate();

            // these two should always equal
            Assert.AreEqual(error1, error2, 0.0001);


            return error2;
        }
    }
}
