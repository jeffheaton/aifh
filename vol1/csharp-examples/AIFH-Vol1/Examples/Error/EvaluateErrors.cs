using System;
using AIFH_Vol1.Core.Error;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Examples.Error
{
    /// <summary>
    /// Example that demonstrates how to calculate errors.  This allows you to see how different 
    /// types of distortion affect the final error for various error calculation methods.
    ///
    /// Type    ESS     MSE     RMS
    /// Small   52      0.0004  0.0204
    /// Medium  1305    0.0104  0.1022
    /// Large   5223    0.0418  0.2044
    /// Huge    522369  4.1790  2.0442
    ///
    /// </summary>
    public class EvaluateErrors
    {

        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Evaluate several error calculation methods";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 6;

        /// <summary>
        /// The random seed to use.
        /// </summary>
        public const int Seed = 1420;

        /// <summary>
        /// The number of rows.
        /// </summary>
        public const int Rows = 10000;

        /// <summary>
        /// The number of columns.
        /// </summary>
        public const int Cols = 25;

        /// <summary>
        /// The low value.
        /// </summary>
        public const double Low = -1;

        /// <summary>
        /// The high value.
        /// </summary>
        public const double High = 1;

        /// <summary>
        /// Generate random data.
        /// </summary>
        /// <param name="seed">The seed to use.</param>
        /// <param name="rows">The number of rows to generate.</param>
        /// <param name="cols">The number of columns to generate.</param>
        /// <param name="low">The low value.</param>
        /// <param name="high">The high value.</param>
        /// <param name="distort">The distortion factor.</param>
        /// <returns>The data set.</returns>
        public DataHolder Generate(int seed, int rows, int cols, double low,
            double high, double distort)
        {
            IGenerateRandom rnd = new MersenneTwisterGenerateRandom((uint)seed);

            var ideal = new double[rows][];
            var actual = new double[rows][];

            for (int row = 0; row < rows; row++)
            {
                for (int col = 0; col < cols; col++)
                {
                    ideal[row] = new double[cols];
                    actual[row] = new double[cols];
                    ideal[row][col] = rnd.NextDouble(low, high);
                    actual[row][col] = ideal[row][col] + (rnd.NextGaussian() * distort);
                }
            }

            var result = new DataHolder {Actual = actual, Ideal = ideal};
            return result;
        }

        /// <summary>
        /// Run the example.
        /// </summary>
        public void Process()
        {

            IErrorCalculation calcEss = new ErrorCalculationSSE();
            IErrorCalculation calcMse = new ErrorCalculationMSE();
            IErrorCalculation calcRms = new ErrorCalculationRMS();

            DataHolder smallErrors = Generate(Seed, Rows, Cols, Low, High, 0.1);
            DataHolder mediumErrors = Generate(Seed, Rows, Cols, Low, High, 0.5);
            DataHolder largeErrors = Generate(Seed, Rows, Cols, Low, High, 1.0);
            DataHolder hugeErrors = Generate(Seed, Rows, Cols, Low, High, 10.0);

            double smallEss = smallErrors.CalculateError(calcEss);
            double smallMse = smallErrors.CalculateError(calcMse);
            double smallRms = smallErrors.CalculateError(calcRms);

            double mediumEss = mediumErrors.CalculateError(calcEss);
            double mediumMse = mediumErrors.CalculateError(calcMse);
            double mediumRms = mediumErrors.CalculateError(calcRms);

            double largeEss = largeErrors.CalculateError(calcEss);
            double largeMse = largeErrors.CalculateError(calcMse);
            double largeRms = largeErrors.CalculateError(calcRms);

            double hugeEss = hugeErrors.CalculateError(calcEss);
            double hugeMse = hugeErrors.CalculateError(calcMse);
            double hugeRms = hugeErrors.CalculateError(calcRms);

            Console.WriteLine("Type\tESS\tMSE\tRMS");
            Console.WriteLine("Small\t" + (int)smallEss + "\t" + smallMse.ToString("0.0000") + "\t" + smallRms.ToString("0.0000"));
            Console.WriteLine("Medium\t" + (int)mediumEss + "\t" + mediumMse.ToString("0.0000") + "\t" + mediumRms.ToString("0.0000"));
            Console.WriteLine("Large\t" + (int)largeEss + "\t" + largeMse.ToString("0.0000") + "\t" + largeRms.ToString("0.0000"));
            Console.WriteLine("Huge\t" + (int)hugeEss + "\t" + hugeMse.ToString("0.0000") + "\t" + hugeRms.ToString("0.0000"));

        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new EvaluateErrors();
            prg.Process();
        }
    }
}
