using System;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Examples.Randomize
{
    /// <summary>
    /// Approximate PI by Monte Carlo.
    /// 
    /// http://en.wikipedia.org/wiki/Monte_Carlo_method
    /// </summary>
    public class EvaluatePI
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Approximate PI by Monte Carlo";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 4;

        /// <summary>
        /// Random number generator.
        /// </summary>
        private IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        public void Process()
        {
            long tries = 0;
            int success = 0;
            int lastUpdate = 0;

            for (int i = 0; i < 1000000000; i++)
            {
                // pick a point at random.
                double x = _rnd.NextDouble();
                double y = _rnd.NextDouble();

                tries++;

                // was the point inside of a circle?
                if (x * x + y * y <= 1)
                    success++;

                lastUpdate++;
                if (lastUpdate >= 1000000)
                {
                    double pi = 4 * (double)success / tries;
                    Console.WriteLine("Tries=" + tries + ", pi=" + pi);
                    lastUpdate = 0;
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
            var program = new EvaluatePI();
            program.Process();
        }
    }
}
