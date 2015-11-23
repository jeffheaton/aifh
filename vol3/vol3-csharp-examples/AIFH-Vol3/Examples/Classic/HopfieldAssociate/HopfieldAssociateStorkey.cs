using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3_Core.Core.Energetic;

namespace AIFH_Vol3.Examples.Classic.HopfieldAssociate
{
    /// <summary>
    ///  * Simple class to recognize some patterns with a Hopfield Neural Network.
    /// This version makes use of the storkey training.
    ///
    ///
    /// This is very loosely based on a an example by Karsten Kutza,
    /// written in C on 1996-01-30. (link below is no longer active)
    /// http://www.neural-networks-at-your-fingertips.com/hopfield.html
    ///
    /// I translated it to Java and adapted it to use Encog for neural
    /// network processing.I mainly kept the patterns from the
    /// original example.
    /// </summary>
    public class HopfieldAssociateStorkey
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Hopfield Associate - Storkey.";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 3;

        public const int HEIGHT = 10;
        public const int WIDTH = 10;

        /**
         * The neural network will learn these patterns.
         */

        public readonly static String[][] PATTERN =
        {
            new string[]
            {
                "O O O O O ",
                " O O O O O",
                "O O O O O ",
                " O O O O O",
                "O O O O O ",
                " O O O O O",
                "O O O O O ",
                " O O O O O",
                "O O O O O ",
                " O O O O O"
            },

            new string[]
            {
                "OO  OO  OO",
                "OO  OO  OO",
                "  OO  OO  ",
                "  OO  OO  ",
                "OO  OO  OO",
                "OO  OO  OO",
                "  OO  OO  ",
                "  OO  OO  ",
                "OO  OO  OO",
                "OO  OO  OO"
            },
            new string[]
            {
                "OOOOO     ",
                "OOOOO     ",
                "OOOOO     ",
                "OOOOO     ",
                "OOOOO     ",
                "     OOOOO",
                "     OOOOO",
                "     OOOOO",
                "     OOOOO",
                "     OOOOO"
            },
            new string[]
            {
                "O  O  O  O",
                " O  O  O  ",
                "  O  O  O ",
                "O  O  O  O",
                " O  O  O  ",
                "  O  O  O ",
                "O  O  O  O",
                " O  O  O  ",
                "  O  O  O ",
                "O  O  O  O"
            },
            new string[]
            {
                "OOOOOOOOOO",
                "O        O",
                "O OOOOOO O",
                "O O    O O",
                "O O OO O O",
                "O O OO O O",
                "O O    O O",
                "O OOOOOO O",
                "O        O",
                "OOOOOOOOOO"
            }
        };

        /**
         * The neural network will be tested on these patterns, to see
         * which of the last set they are the closest to.
         */

        public static readonly String[][]
            PATTERN2 =
            {
                new string[]
                {
                    "          ",
                    "          ",
                    "          ",
                    "          ",
                    "          ",
                    " O O O O O",
                    "O O O O O ",
                    " O O O O O",
                    "O O O O O ",
                    " O O O O O"
                },

                new string[]
                {
                    "OOO O    O",
                    " O  OOO OO",
                    "  O O OO O",
                    " OOO   O  ",
                    "OO  O  OOO",
                    " O OOO   O",
                    "O OO  O  O",
                    "   O OOO  ",
                    "OO OOO  O ",
                    " O  O  OOO"
                },

                new string[]
                {
                    "OOOOO     ",
                    "O   O OOO ",
                    "O   O OOO ",
                    "O   O OOO ",
                    "OOOOO     ",
                    "     OOOOO",
                    " OOO O   O",
                    " OOO O   O",
                    " OOO O   O",
                    "     OOOOO"
                },
                new string[]
                {
                    "O  OOOO  O",
                    "OO  OOOO  ",
                    "OOO  OOOO ",
                    "OOOO  OOOO",
                    " OOOO  OOO",
                    "  OOOO  OO",
                    "O  OOOO  O",
                    "OO  OOOO  ",
                    "OOO  OOOO ",
                    "OOOO  OOOO"
                },
                new string[]
                {
                    "OOOOOOOOOO",
                    "O        O",
                    "O        O",
                    "O        O",
                    "O   OO   O",
                    "O   OO   O",
                    "O        O",
                    "O        O",
                    "O        O",
                    "OOOOOOOOOO"
                }
            };

        public static double[] ConvertPattern(String[][] data, int index)
        {
            int resultIndex = 0;
            double[] result = new double[WIDTH * HEIGHT];
            for (int row = 0; row < HEIGHT; row++)
            {
                for (int col = 0; col < WIDTH; col++)
                {
                    char ch = data[index][row][col];
                    result[resultIndex++] = (ch != ' ') ? 1 : -1;
                }
            }
            return result;
        }

        public static void Display(double[] pattern1, double[] pattern2)
        {
            int index1 = 0;
            int index2 = 0;

            for (int row = 0; row < HEIGHT; row++)
            {
                StringBuilder line = new StringBuilder();

                for (int col = 0; col < WIDTH; col++)
                {
                    if (pattern1[index1++] > 0)
                        line.Append('O');
                    else
                        line.Append(' ');
                }

                line.Append("   ->   ");

                for (int col = 0; col < WIDTH; col++)
                {
                    if (pattern2[index2++] > 0)
                        line.Append('O');
                    else
                        line.Append(' ');
                }

                Console.WriteLine(line.ToString());
            }
        }


        public static void Evaluate(HopfieldNetwork hopfieldLogic, String[][] pattern)
        {
            for (int i = 0; i < pattern.Length; i++)
            {
                double[] pattern1 = ConvertPattern(pattern, i);
                hopfieldLogic.CopyToCurrentState(pattern1);
                int cycles = hopfieldLogic.RunUntilStable(100);
                double[] pattern2 = hopfieldLogic.CurrentState;
                Console.WriteLine("Cycles until stable(max 100): " + cycles + ", result=");
                Display(pattern1, pattern2);
                Console.WriteLine("----------------------");
            }
        }

        public void Run()
        {
            HopfieldNetwork hopfieldLogic = new HopfieldNetwork(WIDTH * HEIGHT);
            TrainHopfieldStorkey train = new TrainHopfieldStorkey(hopfieldLogic);

            for (int i = 0; i < PATTERN.Length; i++)
            {
                train.AddPattern(ConvertPattern(PATTERN, i));
            }

            Evaluate(hopfieldLogic, PATTERN);
            Evaluate(hopfieldLogic, PATTERN2);
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            {
                HopfieldAssociateHebbian program = new HopfieldAssociateHebbian();
                program.Run();
            }
        }
    }
}
