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
using System.IO;
using System.Reflection;
using CsvHelper;

namespace AIFH_Vol3.Examples.Intro
{
    /// <summary>
    ///     Read a CSV file using CsvHelper.  Make sure that you have CsvHelper installed as
    ///     a reference using NuGET.
    /// </summary>
    public class CSVExample
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Shows how to use CsvHelper to read a CSV";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 1;

        /// <summary>
        ///     Run the example.
        /// </summary>
        public void Run()
        {
            // read the iris data from the resources
            var assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol3.Resources.iris.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read iris data from embedded resources.");
                return;
            }

            var istream = new StreamReader(res);
            using (var reader = new CsvReader(istream))
            {
                var firstLine = true;
                while (reader.Read())
                {
                    // if it is the first line, then display the headers first.
                    if (firstLine)
                    {
                        var first = true;
                        foreach (var str in reader.FieldHeaders)
                        {
                            if (!first)
                            {
                                Console.Write(",");
                            }
                            Console.Write(str);
                            first = false;
                        }
                        Console.WriteLine();
                    }

                    // write out a line
                    var first2 = true;
                    foreach (var str in reader.CurrentRecord)
                    {
                        if (!first2)
                        {
                            Console.Write(",");
                        }
                        Console.Write(str);
                        first2 = false;
                    }
                    Console.WriteLine();

                    firstLine = false;
                }
            }
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new CSVExample();
            prg.Run();
        }
    }
}