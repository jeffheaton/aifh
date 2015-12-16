using AIFH_Vol3_Core.Core.Selection;
using AIFH_Vol3_Core.Core.Util;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.Search
{
    public class ListSearches
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "List model searches: random and grid.";

        /// <summary>
        /// The chapter this example is from.
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
            GridModelSelection grid = new GridModelSelection();
            grid.AddCategoryAxis(new String[] { "sigmoid", "ReLU", "tanh" });
            grid.AddNumericAxis(1, 10, 1);
            Object[] list;

            while ((list = grid.Next()) != null)
            {
                Console.WriteLine(ArrayUtil.List2String(list));
            }

            Console.WriteLine("Random Search");

            RandomModelSelection rnd = new RandomModelSelection();
            rnd.AddCategoryAxis(new String[] { "sigmoid", "ReLU", "tanh" });
            rnd.AddNumericAxis(1, 10, 1);

            for (int i = 0; i < 10; i++)
            {
                Console.WriteLine(ArrayUtil.List2String(rnd.Next()));
            }
        }
    }
}
