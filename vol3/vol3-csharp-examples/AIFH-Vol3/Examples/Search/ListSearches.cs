// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
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
using AIFH_Vol3_Core.Core.Selection;
using AIFH_Vol3_Core.Core.Util;

namespace AIFH_Vol3.Examples.Search
{
    public class ListSearches
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "List model searches: random and grid.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 11;

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            Console.WriteLine("Grid Search");
            var grid = new GridModelSelection();
            grid.AddCategoryAxis(new[] {"sigmoid", "ReLU", "tanh"});
            grid.AddNumericAxis(1, 10, 1);
            object[] list;

            while ((list = grid.Next()) != null)
            {
                Console.WriteLine(ArrayUtil.List2String(list));
            }

            Console.WriteLine("Random Search");

            var rnd = new RandomModelSelection();
            rnd.AddCategoryAxis(new[] {"sigmoid", "ReLU", "tanh"});
            rnd.AddNumericAxis(1, 10, 1);

            for (var i = 0; i < 10; i++)
            {
                Console.WriteLine(ArrayUtil.List2String(rnd.Next()));
            }
        }
    }
}