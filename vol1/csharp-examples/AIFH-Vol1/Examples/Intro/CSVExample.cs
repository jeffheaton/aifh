using System;
using System.IO;
using System.Reflection;
using CsvHelper;

namespace AIFH_Vol1.Examples.Intro
{
    /// <summary>
    /// Read a CSV file using CsvHelper.  Make sure that you have CsvHelper installed as
    /// a reference using NuGET.
    /// </summary>
    public class CSVExample
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Shows how to use CsvHelper to read a CSV";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 1;

        /// <summary>
        /// Run the example.
        /// </summary>
        public void Run()
        {
            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol1.Resources.iris.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read iris data from embedded resources.");
                return;
            }

            var istream = new StreamReader(res);
            using (var reader = new CsvReader(istream))
            {
                bool firstLine = true;
                while (reader.Read())
                {
                    // if it is the first line, then display the headers first.
                    if (firstLine)
                    {
                        bool first = true;
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
                    bool first2 = true;
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
