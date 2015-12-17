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
using System.Text;
using AIFH_Vol3_Core.Core.Energetic;

namespace AIFH_Vol3.Examples.Classic.HopfieldAssociate
{
    /// <summary>
    ///     * Simple class to recognize some patterns with a Hopfield Neural Network.
    ///     This version makes use of the storkey training.
    ///     This is very loosely based on a an example by Karsten Kutza,
    ///     written in C on 1996-01-30. (link below is no longer active)
    ///     http://www.neural-networks-at-your-fingertips.com/hopfield.html
    ///     I translated it to Java and adapted it to use Encog for neural
    ///     network processing.I mainly kept the patterns from the
    ///     original example.
    /// </summary>
    public class HopfieldAssociateStorkey
    {
        public const int HEIGHT = 10;
        public const int WIDTH = 10;

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Hopfield Associate - Storkey.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 3;

        /**
         * The neural network will learn these patterns.
         */

        public static readonly string[][] PATTERN =
        {
            new[]
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
            new[]
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
            new[]
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
            new[]
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
            new[]
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

        public static readonly string[][]
            PATTERN2 =
            {
                new[]
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
                new[]
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
                new[]
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
                new[]
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
                new[]
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

        public static double[] ConvertPattern(string[][] data, int index)
        {
            var resultIndex = 0;
            var result = new double[WIDTH*HEIGHT];
            for (var row = 0; row < HEIGHT; row++)
            {
                for (var col = 0; col < WIDTH; col++)
                {
                    var ch = data[index][row][col];
                    result[resultIndex++] = ch != ' ' ? 1 : -1;
                }
            }
            return result;
        }

        public static void Display(double[] pattern1, double[] pattern2)
        {
            var index1 = 0;
            var index2 = 0;

            for (var row = 0; row < HEIGHT; row++)
            {
                var line = new StringBuilder();

                for (var col = 0; col < WIDTH; col++)
                {
                    if (pattern1[index1++] > 0)
                        line.Append('O');
                    else
                        line.Append(' ');
                }

                line.Append("   ->   ");

                for (var col = 0; col < WIDTH; col++)
                {
                    if (pattern2[index2++] > 0)
                        line.Append('O');
                    else
                        line.Append(' ');
                }

                Console.WriteLine(line.ToString());
            }
        }


        public static void Evaluate(HopfieldNetwork hopfieldLogic, string[][] pattern)
        {
            for (var i = 0; i < pattern.Length; i++)
            {
                var pattern1 = ConvertPattern(pattern, i);
                hopfieldLogic.CopyToCurrentState(pattern1);
                var cycles = hopfieldLogic.RunUntilStable(100);
                var pattern2 = hopfieldLogic.CurrentState;
                Console.WriteLine("Cycles until stable(max 100): " + cycles + ", result=");
                Display(pattern1, pattern2);
                Console.WriteLine("----------------------");
            }
        }

        public void Run()
        {
            var hopfieldLogic = new HopfieldNetwork(WIDTH*HEIGHT);
            var train = new TrainHopfieldStorkey(hopfieldLogic);

            for (var i = 0; i < PATTERN.Length; i++)
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
                var program = new HopfieldAssociateHebbian();
                program.Run();
            }
        }
    }
}