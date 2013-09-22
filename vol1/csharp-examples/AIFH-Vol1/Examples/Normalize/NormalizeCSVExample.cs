using System;
using System.IO;
using System.Reflection;
using AIFH_Vol1.Core.Normalize;

namespace AIFH_Vol1.Examples.Normalize
{
    /// <summary>
    /// A simple normalization example for the Iris data set.
    /// </summary>
    public class NormalizeCSVExample
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "A simple normalization example for the Iris data set";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 2;

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new NormalizeCSVExample();
            prg.Run();
        }

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

            // load the data
            var istream = new StreamReader(res);
            DataSet ds = DataSet.Load(istream);
            istream.Close();

            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.NormalizeRange(0, 0, 1);
            ds.NormalizeRange(1, 0, 1);
            ds.NormalizeRange(2, 0, 1);
            ds.NormalizeRange(3, 0, 1);
            ds.EncodeEquilateral(4);

            DataSet.Save("normalized.csv", ds);
            Console.WriteLine("Output written to: normalized.csv");

        }
    }

}
